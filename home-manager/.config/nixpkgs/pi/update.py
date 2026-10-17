#!/usr/bin/env python3
"""Update Pi sources and npm locks; run through `nix run .#update-pi`."""

import argparse
import base64
import hashlib
import io
import json
import os
from pathlib import Path
import re
import subprocess
import tarfile
import tempfile
import urllib.parse
import urllib.request

GIT_INPUTS = ("pi", "pi-tool-summaries")
NPM_REGISTRIES = {
    "npmjs": "https://registry.npmjs.org",
    "yarnpkg": "https://registry.yarnpkg.com",
}
# input name -> scoped attribute, derivation filename, keep dev dependencies
NPM = {
    "pi-subagents": ("subagents", "pi-subagents", False),
    "pi-btw": ("btw", "pi-btw", False),
    "pi-codex-goal": ("codex-goal", "pi-codex-goal", False),
    "pi-web-access": ("web-access", "pi-web-access", False),
    "context-mode": ("context-mode", "pi-context-mode", True),
    "pi-background-tasks": ("background-tasks", "pi-background-tasks", False),
}


def run(*args, capture=False, **kwargs):
    print("+", " ".join(map(str, args)), flush=True)
    return subprocess.run(
        args,
        check=True,
        text=True,
        stdout=subprocess.PIPE if capture else None,
        **kwargs,
    ).stdout


def npm_environment(directory, registry):
    """Use the selected public registry without inheriting ambient npm settings."""
    env = {
        key: value
        for key, value in os.environ.items()
        if not key.lower().startswith("npm_config_")
    }
    env.update(
        {
            "npm_config_cache": str(directory / "npm-cache"),
            "npm_config_globalconfig": str(directory / "global.npmrc"),
            "npm_config_registry": registry + "/",
            "npm_config_replace_registry_host": "always",
            "npm_config_userconfig": str(directory / "user.npmrc"),
        }
    )
    return env


def normalize_package_lock(content, registry):
    """Keep public registry URLs in a generated lock on the selected provider."""
    lock = json.loads(content)
    registry_hosts = {
        urllib.parse.urlsplit(url).netloc for url in NPM_REGISTRIES.values()
    }
    target = urllib.parse.urlsplit(registry)
    for package in lock.get("packages", {}).values():
        if not isinstance(package, dict) or "resolved" not in package:
            continue
        resolved = urllib.parse.urlsplit(package["resolved"])
        if resolved.netloc in registry_hosts:
            package["resolved"] = urllib.parse.urlunsplit(
                (
                    target.scheme,
                    target.netloc,
                    resolved.path,
                    resolved.query,
                    resolved.fragment,
                )
            )
    return json.dumps(lock, indent=2) + "\n"


def replace_once(text, pattern, replacement):
    if len(re.findall(pattern, text, re.MULTILINE)) != 1:
        raise ValueError(f"Expected exactly one match: {pattern}")
    return re.sub(pattern, lambda _: replacement, text, count=1, flags=re.MULTILINE)


def download(url):
    with urllib.request.urlopen(url, timeout=60) as response:
        return response.read()


def npm_tarball_url(registry, name, version):
    archive_name = name.rsplit("/", 1)[-1]
    return f"{registry}/{name}/-/{archive_name}-{version}.tgz"


def npm_tarball_pattern(name, version_pattern):
    registries = "|".join(re.escape(url) for url in NPM_REGISTRIES.values())
    archive_name = name.rsplit("/", 1)[-1]
    return (
        rf"(?:{registries})/{re.escape(name)}/-/"
        rf"{re.escape(archive_name)}-{version_pattern}\.tgz"
    )


def current_version(name, flake, recipe):
    pattern = (
        r'version = "([^"]+)";'
        if name == "pi-background-tasks"
        else npm_tarball_pattern(name, r'([^"/]+)')
    )
    matches = re.findall(pattern, recipe if name == "pi-background-tasks" else flake)
    if len(matches) != 1:
        raise ValueError(f"Cannot determine pinned version for {name}")
    return matches[0]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--package",
        action="append",
        choices=(*GIT_INPUTS, *NPM),
        help="Update only this package/input; repeatable",
    )
    parser.add_argument(
        "--registry",
        choices=NPM_REGISTRIES,
        default="npmjs",
        help="Public npm registry provider used for metadata, tarballs, and locks",
    )
    parser.add_argument(
        "--keep-versions",
        action="store_true",
        help="Regenerate npm locks at current versions; do not advance Git inputs",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Check pinned npm versions against managed locks, offline, without writing",
    )
    parser.add_argument(
        "--no-build",
        action="store_true",
        help="Leave build validation to the caller (not recommended)",
    )
    args = parser.parse_args()
    registry = NPM_REGISTRIES[args.registry]
    root = Path(run("git", "rev-parse", "--show-toplevel", capture=True).strip())
    base = root / "home-manager/.config/nixpkgs/pi"
    flake_path = root / "flake.nix"
    flake = flake_path.read_text()
    selected = set(args.package or (*GIT_INPUTS, *NPM))
    paths = [flake_path, root / "flake.lock"]
    for name in selected & NPM.keys():
        paths += [base / "packages" / (NPM[name][1] + ".nix")]
        lock = base / "packages" / (name + "-package-lock.json")
        if lock.exists():
            paths.append(lock)
    if not args.check:
        dirty = run(
            "git",
            "status",
            "--porcelain",
            "--",
            *map(str, paths),
            capture=True,
            cwd=root,
        )
        if dirty.strip():
            raise ValueError(
                "Commit or stash changes to update targets first:\n" + dirty
            )

    changes = {}
    for name in sorted(selected & NPM.keys()):
        _, filename, keep_dev = NPM[name]
        recipe_path = base / "packages" / (filename + ".nix")
        recipe = recipe_path.read_text()
        version = current_version(name, flake, recipe)
        lock_path = base / "packages" / (name + "-package-lock.json")
        if args.check:
            if lock_path.exists():
                lock = json.loads(lock_path.read_text())
                if lock["packages"][""]["version"] != version:
                    raise ValueError(f"{name}: source and lock versions differ")
            print(f"{name}: {version} OK")
            continue
        selector = version if args.keep_versions else "latest"
        encoded_name = urllib.parse.quote(name, safe="@")
        metadata = json.loads(download(f"{registry}/{encoded_name}/{selector}"))
        version = metadata["version"]
        tarball_url = npm_tarball_url(registry, name, version)
        archive = download(tarball_url)
        algorithm, digest = metadata["dist"]["integrity"].split("-", 1)
        if (
            base64.b64encode(hashlib.new(algorithm, archive).digest()).decode()
            != digest
        ):
            raise ValueError(f"{name}: tarball integrity mismatch")
        with tarfile.open(fileobj=io.BytesIO(archive), mode="r:gz") as tar:
            manifest = json.load(tar.extractfile("package/package.json"))
        if manifest["name"] != name or manifest["version"] != version:
            raise ValueError(f"{name}: tarball manifest mismatch")
        if name == "pi-background-tasks":
            recipe = replace_once(
                recipe, r'  version = "[^"]+";', f'  version = "{version}";'
            )
            recipe = replace_once(
                recipe,
                npm_tarball_pattern(name, r'[^"/]+'),
                tarball_url,
            )
            recipe = replace_once(
                recipe,
                r'    hash = "[^"]+";',
                f'    hash = "{metadata["dist"]["integrity"]}";',
            )
        else:
            flake = replace_once(
                flake,
                npm_tarball_pattern(name, r'[^"/]+'),
                tarball_url,
            )
        if lock_path.exists():
            if not keep_dev:
                manifest.pop("devDependencies", None)
            with tempfile.TemporaryDirectory(prefix="pi-npm-lock-") as tmp:
                tmp = Path(tmp)
                (tmp / "package.json").write_text(json.dumps(manifest, indent=2) + "\n")
                # Re-resolve only this package's dependency constraints. Do not
                # execute lifecycle scripts or install private Pi peer copies.
                run(
                    "npm",
                    "install",
                    "--package-lock-only",
                    "--ignore-scripts",
                    "--legacy-peer-deps",
                    "--no-audit",
                    "--no-fund",
                    cwd=tmp,
                    env=npm_environment(tmp, registry),
                )
                generated = tmp / "package-lock.json"
                changes[lock_path] = normalize_package_lock(
                    generated.read_text(), registry
                )
        changes[recipe_path] = recipe
    if args.check:
        return

    # Do not edit the checkout until every selected npm package is prepared.
    changes[flake_path] = flake
    for path, content in changes.items():
        path.write_text(content)
    inputs = sorted((selected & set(GIT_INPUTS)) if not args.keep_versions else set())
    inputs += sorted((selected & NPM.keys()) - {"pi-background-tasks"})
    if inputs:
        run("nix", "flake", "update", *inputs, cwd=root)
    if not args.no_build:
        run("nix", "flake", "check", "--all-systems", "--no-build", cwd=root)
        # Build against the same host runtime the module selects. Also build
        # every extension after a core update, not just packages whose pin moved.
        attrs = [
            "background-tasks",
            "subagents",
            "tool-summaries",
            "btw",
            "codex-goal",
            "web-access",
            "context-mode",
        ]
        expression = (
            "let f = builtins.getFlake (toString ./.); "
            'h = f.lib.mkHomeConfiguration { system = builtins.currentSystem; profile = "headless"; }; '
            "p = h.pkgs.piPackages; in [ p.runtime.package "
            + " ".join("p." + attr for attr in attrs)
            + " ]"
        )
        run("nix", "build", "--no-link", "--impure", "--expr", expression, cwd=root)
    print(
        "Updates prepared. Review git diff; commit only after validation. No activation or push performed."
    )


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        raise SystemExit(str(error))
