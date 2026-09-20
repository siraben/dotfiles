#!/usr/bin/env python3
"""Compare configured package versions at two Git commits, without building them."""

import argparse
import collections
import html
import json
from pathlib import Path
import subprocess
import tarfile
import tempfile

START = "<!-- package-manifest:start -->"
END = "<!-- package-manifest:end -->"


def run(*args, **kwargs):
    return subprocess.check_output(args, text=True, **kwargs).strip()


def manifest(ref, repo):
    revision = run("git", "rev-parse", "--verify", f"{ref}^{{commit}}", cwd=repo)
    with tempfile.TemporaryDirectory(prefix="package-manifest-") as directory:
        root = Path(directory)
        archive = root / "source.tar"
        subprocess.run(["git", "archive", "--output", str(archive), revision], cwd=repo, check=True)
        source = root / "source"
        source.mkdir()
        with tarfile.open(archive) as contents:
            contents.extractall(source, filter="data")
        print(f"Evaluating package manifest for {revision}", flush=True)
        result = run(
            "nix", "eval", "--json", "--impure", "--no-write-lock-file",
            "--file", str(Path(__file__).with_suffix(".nix")),
            "--apply", "f: f { flakePath = " + json.dumps(str(source)).replace("${", "\\${") + "; }", cwd=repo,
        )
    return {"revision": revision, "configurations": json.loads(result)}


def index(packages):
    result = collections.defaultdict(set)
    for package in packages:
        result[package["name"]].add(package["version"])
    return {name: tuple(sorted(versions)) for name, versions in result.items()}


def changes(before, after):
    grouped = collections.defaultdict(list)
    old_configs = before["configurations"]
    new_configs = after["configurations"]
    for config in sorted(old_configs.keys() | new_configs.keys()):
        old = index(old_configs.get(config, []))
        new = index(new_configs.get(config, []))
        for name in sorted(old.keys() | new.keys(), key=str.casefold):
            previous, current = old.get(name, ()), new.get(name, ())
            if previous != current:
                grouped[(name, previous, current)].append(config)
    return grouped


def cell(value):
    return html.escape(value).replace("|", "&#124;").replace("\n", " ").replace("\r", " ")


def versions(values):
    return ", ".join(cell(value) if value else "(unversioned)" for value in values) if values else "—"


def report(before, after):
    delta = changes(before, after)
    updated = sum(bool(old and new) for _, old, new in delta)
    added = sum(not old for _, old, _ in delta)
    removed = sum(not new for _, _, new in delta)
    lines = [
        "### Package changes", "",
        f"Compared `{before['revision'][:12]}` → `{after['revision'][:12]}`.", "",
        f"**{updated} version changes, {added} additions, {removed} removals** "
        "(identical changes across configurations are grouped).", "",
        "Evaluated Home Manager packages and NixOS system packages, kernels, fonts, "
        "and embedded Home Manager profiles. Includes overlays; excludes transitive "
        "dependencies, service-only packages, and rebuilds with unchanged versions. "
        "This is a version comparison, not a build check.", "",
    ]
    if delta:
        lines += ["| Package | Previous | Updated | Configurations |", "| --- | --- | --- | --- |"]
        for (name, old, new), configs in sorted(delta.items(), key=lambda item: (item[0][0].casefold(), item[0][1:])):
            lines.append(f"| {cell(name)} | {versions(old)} | {versions(new)} | {'<br>'.join(map(cell, configs))} |")
    else:
        lines.append("No configured package versions changed.")
    lines += ["", "<details>", "<summary>Compared configurations</summary>", ""]
    for config in sorted(before["configurations"].keys() | after["configurations"].keys()):
        old_count = len(index(before["configurations"].get(config, [])))
        new_count = len(index(after["configurations"].get(config, [])))
        lines.append(f"- {cell(config)}: {old_count} → {new_count} package names")
    lines += ["", "</details>", ""]
    return "\n".join(lines)


def update_body(body, markdown, artifact_url):
    if START in body or END in body:
        if body.count(START) != 1 or body.count(END) != 1 or body.index(START) > body.index(END):
            raise ValueError("Malformed package-manifest markers in PR body")
        start, rest = body.split(START, 1)
        _, end = rest.split(END, 1)
    else:
        start, end = body.rstrip() + "\n\n", ""
    link = f"\nFull manifests and report: [workflow artifacts]({artifact_url}).\n" if artifact_url else ""
    result = start + START + "\n" + markdown + link + END + end
    # GitHub PR bodies have a size limit. Never silently drop the complete report:
    # upload it as an artifact and keep an explicit summary in the description.
    if len(result.encode("utf-8")) > 60_000:
        if not artifact_url:
            raise ValueError("Report too large for a PR body; provide --artifact-url")
        summary = markdown.split("| Package |", 1)[0]
        result = start + START + "\n" + summary + "\nThe package table is too large for the PR description.\n" + link + END + end
    if len(result.encode("utf-8")) > 60_000:
        raise ValueError("Existing PR description is too large to append the package summary")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", required=True, help="Previous Git commit")
    parser.add_argument("--head", required=True, help="Updated Git commit")
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--pr-body", type=Path, help="Existing PR body; writes pr-body.md without publishing")
    parser.add_argument("--artifact-url", default="")
    args = parser.parse_args()
    repo = run("git", "rev-parse", "--show-toplevel")
    before = manifest(args.base, repo)
    after = manifest(args.head, repo)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for name, data in [("before", before), ("after", after)]:
        (args.output_dir / f"{name}.json").write_text(json.dumps(data, indent=2, sort_keys=True) + "\n")
    markdown = report(before, after)
    (args.output_dir / "report.md").write_text(markdown)
    if args.pr_body:
        (args.output_dir / "pr-body.md").write_text(update_body(args.pr_body.read_text(), markdown, args.artifact_url))
    print(markdown)


if __name__ == "__main__":
    main()
