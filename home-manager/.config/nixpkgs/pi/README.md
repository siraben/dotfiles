# Pi configuration

This directory is the single integration point for Pi. The executable comes
from Pi's upstream flake; Home Manager's `programs.pi-coding-agent` module owns
settings, models, instructions, and the package installation. Local Nix files
only package extensions and compose repository-specific defaults.

Pi is enabled for full and headless profiles and completely absent from minimal
profiles, including skills and standalone extensions.

## Layout

- `modules/`: Home Manager options and generated Pi configuration.
- `packages/`: one derivation per extension or companion executable.
- `build-support.nix`: shared `buildPiExtension` builders and install checks.
- `package-set.nix`: scoped packages and the Pi/Node/SDK runtime descriptor.
- `overlay.nix`: exposes `piPackages` and compatibility aliases.
- `themes/`: managed themes.

Import `modules/` and `overlay.nix` directly; the former compatibility entry
points have been removed.

## Composition

Use typed `siraben.pi` options from an additional Home Manager module:

```nix
{
  siraben.pi = {
    packages = [
      "/absolute/path/to/pi-package"
      {
        source = "/absolute/path/to/shared-package";
        shareWithSubagents = true;
      }
    ];
    extensions = [ "/absolute/path/to/extension.ts" ];
    skills = [ "/absolute/path/to/skill" ];
    prompts = [ "/absolute/path/to/prompt.md" ];
    themes = [ "/absolute/path/to/theme.json" ];
    subagentExtensions = [ "/absolute/path/to/subagent-extension.ts" ];
    instructions = "Keep changes focused and report verification.";
  };
}
```

Package objects support Pi's `source`, `autoload`, `extensions`, `skills`,
`prompts`, and `themes` fields. `shareWithSubagents` is a module-only field and
is never written to Pi settings. Sharing defaults to false and inactive
packages are never shared. This keeps subagent behavior explicit rather than
deriving it from store-name substrings or every managed extension file.

`settings.packages` replaces the bundled package list; `siraben.pi.packages`
still appends. Likewise, standalone resource options append to matching raw
settings arrays. A raw `settings.subagents.defaultExtensions` remains an
explicit replacement.

## MCP configuration and credentials

`siraben.pi.mcpServers` declaratively generates `mcp.json`. Optional Cua Driver
configuration and the disabled signed-client `computer-use` endpoint are merged
into that file. Pi no longer reads or translates mutable Codex configuration at
activation time.

Nix store files are readable by local users. Never put credential literals in
Nix options. Refer to runtime environment variables instead:

```nix
siraben.pi.mcpServers.example = {
  url = "https://example.invalid/mcp";
  headers.Authorization = "Bearer \${EXAMPLE_TOKEN}";
};
```

OAuth state, sessions, caches, and secret storage remain unmanaged.

## Packaging policy

`pkgs.piPackages` is a `lib.makeScope` package set: `subagents`,
`background-tasks`, `tool-summaries`, `codex-goal`, `web-access`, `context-mode`,
and the companion `cua-driver`. Existing top-level aliases remain compatible.
Packages use the scope's `callPackage`; builders use `lib.extendMkDerivation`.
Each extension exposes `piPackageRoot` so consumers do not reconstruct its path.

`piPackages.runtime` is the single selection point for the host `package`,
`nodejs`, and `sdkRoot`. The default describes the official upstream Pi package
and Node 22. The module uses that host and extension builds use that Node/SDK.
When changing Pi distributions, override all three together; a standalone Bun
binary does not necessarily provide an SDK tree. For example:

```nix
{ lib, ... }: {
  # Apply after the base module's overlay, which defines piPackages.
  nixpkgs.overlays = lib.mkAfter [ (final: prev: {
    piPackages = prev.piPackages.overrideScope (_self: _super: {
      runtime = {
        package = myPi;
        nodejs = myPiNode;
        sdkRoot = myPiSdkRoot;
      };
    });
  }) ];
}
```

Builders validate the installed Pi manifest, with focused runtime checks where
needed. Install phases retain published/runtime content rather than npm build
worktrees. Background tasks uses `importNpmLock` and its checked-in integrity
hashes instead of a second `npmDepsHash`. Subagents also uses `importNpmLock`;
the other npm builds retain their existing cache hashes and native build paths.

`piPackages.context-mode` (also `pkgs.pi-context-mode`) includes the Pi adapter;
nixpkgs's MCP-server-only `context-mode` remains unshadowed. The Pi package is
configured with `autoload = false`.
Its native SQLite fallback is built for Pi's Node ABI, while normal operation
uses Node's FTS5-enabled built-in SQLite.

`pi-tool-renderer` and its unused `kendex` input were removed.

## Updating sources

From this checkout, with update targets committed:

```console
nix run .#update-pi
nix run .#update-pi -- --package pi-background-tasks
nix run .#update-pi -- --package pi-web-access --keep-versions
nix run .#update-pi -- --check
```

The default updates the Pi/core and Git extension inputs, selects npm `latest`
releases, verifies tarball integrity, regenerates managed dependency locks, and
refreshes the two remaining `npmDepsHash` values. It then evaluates the flake
and builds the host plus every bundled extension on the current platform.
It does not activate, commit, or push. Review upstream compatibility and the diff
before committing; successfully resolving peer constraints is not proof of Pi
API compatibility. Native builds on other platforms still belong in CI.

`--package` is repeatable. `--keep-versions` regenerates npm dependency locks
without advancing package versions or Git inputs; transitive dependencies may
still advance within their declared ranges. `--check` checks source/lock version
consistency offline without changing files. `--no-build` explicitly delegates
validation to the caller. The updater prepares npm changes in temporary storage
first, refuses dirty update targets, and leaves prepared changes available for
inspection if flake locking or build validation fails. It never rolls back
unrelated working-tree changes.

Published package manifests supply the lock inputs; lifecycle scripts are not
executed during lock generation. Context Mode retains build dependencies;
background tasks and web access omit development dependencies, matching their
package recipes. Pi peers are not installed by npm's legacy-peer resolver.
Git source locks remain upstream-owned. This is deliberately a small updater
for this collection, not a general Pi registry crawler.

The expensive-scan extension is a best-effort mistake-prevention guard, not a
security sandbox. Dynamic shell expansion and arbitrary interpreters require an
actual OS sandbox when they must be prohibited.
