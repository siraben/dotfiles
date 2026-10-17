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
- `package-set.nix`: scoped packages and the Pi/Node runtime descriptor.
- `overlay.nix`: exposes the scoped `piPackages` set and top-level aliases.
- `themes/`: managed themes.

Import `modules/` and `overlay.nix` directly.

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

`settings` is a freeform JSON submodule with typed package and resource lists.
Invalid entries are rejected by the Nix module system. Omitted or null resource
lists leave defaults intact; `settings.packages = []` disables bundled packages.
`settings.packages` replaces the bundled package list, and
`siraben.pi.packages` appends to that list. Standalone resource options append
to matching raw settings arrays. A raw `settings.subagents.defaultExtensions`
replaces the generated list.

## MCP configuration and credentials

`siraben.pi.mcpServers` declaratively generates `mcp.json`. Enabling Cua Driver
on macOS adds its server definition. Entries in `siraben.pi.mcpServers` merge by
name and can override that definition.

Nix store files are readable by local users. Never put credential literals in
Nix options. Refer to runtime environment variables instead:

```nix
siraben.pi.mcpServers.example = {
  url = "https://example.invalid/mcp";
  headers.Authorization = "Bearer \${EXAMPLE_TOKEN}";
};
```

OAuth state, sessions, caches, and secret storage are outside this module's
scope.

## Packaging policy

`pkgs.piPackages` is a `lib.makeScope` package set: `subagents`,
`background-tasks`, `tool-summaries`, `btw`, `codex-goal`, `web-access`,
`context-mode`, and the companion `cua-driver`. Top-level aliases reference the
same package derivations. Packages use the scope's `callPackage`; builders use
`lib.extendMkDerivation`.
Each extension exposes `piPackageRoot` so consumers do not reconstruct its path.

`piPackages.runtime` is the single selection point for the host `package` and
`nodejs`. The default selects the official upstream Pi package and Node 22.
The module uses that host and native extension dependencies use that Node ABI.
The scope also instantiates nixpkgs's `importNpmLock` hook with the same Node
headers and node-gyp; changing only the executable would not be sufficient.
Published extensions resolve Pi peers through the host; no recipe links private
SDK paths or compiles against an independently installed Pi. When changing Pi
distributions, select the compatible runtime too. For example:

```nix
{ lib, ... }: {
  # Apply after the base module's overlay, which defines piPackages.
  nixpkgs.overlays = lib.mkAfter [ (final: prev: {
    piPackages = prev.piPackages.overrideScope (_self: _super: {
      runtime = {
        package = myPi;
        nodejs = myPiNode;
      };
    });
  }) ];
}
```

Both builders install the file set selected by `npm pack --ignore-scripts` and
perform the same manifest check and cleanup. All npm dependencies use
`importNpmLock` with checked-in integrity hashes; no second `npmDepsHash` is
maintained. Root dependency metadata is matched to the lock with a shared JSON
transform, not per-package `sed` patches.

Subagents installs its published JavaScript release with runtime dependencies
only. Tool summaries and BTW are source-only TypeScript packages loaded by Pi
directly. Context Mode uses package-specific native-addon cleanup and a CLI
wrapper.

`piPackages.context-mode` (also `pkgs.pi-context-mode`) includes the Pi adapter
and is distinct from nixpkgs's MCP-server-only `context-mode`. The Pi package is
configured with `autoload = false`. Its native SQLite fallback is built for Pi's
Node ABI, while normal operation uses Node's FTS5-enabled built-in SQLite.

## Updating sources

From this checkout, with update targets committed:

```console
nix run .#update-pi
nix run .#update-pi -- --package pi-background-tasks
nix run .#update-pi -- --package pi-web-access --keep-versions
nix run .#update-pi -- --registry yarnpkg
nix run .#update-pi -- --check
```

The default updates the Pi/core and Git extension inputs, selects npm `latest`
releases, verifies tarball integrity, and regenerates managed dependency locks.
It then evaluates the flake and builds the host plus every bundled extension on
the current platform. It does not activate, commit, or push. Review upstream
compatibility and the diff before committing; successfully resolving peer
constraints is not proof of Pi API compatibility. CI covers native builds on
other platforms.

`--package` is repeatable. `--registry` selects the allowlisted public npm
provider used for metadata, source tarballs, and generated lock URLs; `npmjs` is
the default and `yarnpkg` is also available. Ambient npm registry settings are
ignored. `--keep-versions` regenerates npm dependency locks without advancing
package versions or Git inputs; transitive dependencies may advance within
their declared ranges. `--check` checks source/lock version consistency offline
without changing files. `--no-build` explicitly delegates validation to the
caller. The updater prepares npm changes in temporary storage first, refuses
dirty update targets, and leaves prepared changes available for inspection if
flake locking or build validation fails. It never rolls back unrelated
working-tree changes.

Published package manifests supply the lock inputs; lifecycle scripts are not
executed during lock generation. Context Mode includes build dependencies;
subagents, background tasks, and web access omit development dependencies to
match their package recipes. Pi peers are not installed by npm's legacy-peer
resolver. Git source locks come from their upstream repositories. `update-pi`
manages only this repository's package collection.

The expensive-scan extension is a best-effort mistake-prevention guard, not a
security sandbox. Dynamic shell expansion and arbitrary interpreters require an
actual OS sandbox when they must be prohibited.
