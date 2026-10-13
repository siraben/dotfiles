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
- `overlay.nix`: a small `callPackage` registry.
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

Most packages use standard Nix arguments and are instantiated with `callPackage`.
`pi-background-tasks` retains its published-bundle derivation and dependency lock.
The shared `buildPiExtension` and `buildPiNpmExtension` helpers follow nixpkgs's
`lib.extendMkDerivation` pattern. They select Node 22 and validate the installed Pi manifest; individual packages only add
checks for their own runtime artifacts. Install phases retain published/runtime
content rather than npm build worktrees.

The local package attribute is `pi-context-mode`; nixpkgs's `context-mode`
attribute remains unshadowed. Context Mode is configured with `autoload = false`.
Its native SQLite fallback is built for Pi's Node ABI, while normal operation
uses Node's FTS5-enabled built-in SQLite.

`pi-tool-renderer` and its unused `kendex` input were removed.

## Updating sources

GitHub extension inputs advance through the flake lock:

```console
nix flake update pi-subagents pi-tool-summaries
```

Published npm inputs (`context-mode`, `pi-codex-goal`, and `pi-web-access`) need
an explicit URL update. Update the matching package lock and `npmDepsHash` for
packages built with npm, then build the affected Home Manager profiles.

The expensive-scan extension is a best-effort mistake-prevention guard, not a
security sandbox. Dynamic shell expansion and arbitrary interpreters require an
actual OS sandbox when they must be prohibited.
