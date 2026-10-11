# Pi configuration

[modules/](modules/) configures upstream Home Manager `programs.pi-coding-agent`.
Use `siraben.pi.settings` for recursive settings overrides, `siraben.pi.providers`
for models, and `siraben.pi.enableCuaDriver` to enable Cua on macOS. Pi is off by
default in minimal profiles. Companion files require the default `~/.pi/agent`
directory; settings, models and instructions are read-only.

Resource contributions from modules merge without replacing bundled defaults:

```nix
siraben.pi = {
  packages = [ "/absolute/path/to/pi-package" ];
  extensions = [ "/absolute/path/to/extension.ts" ];
  skills = [ "/absolute/path/to/skill" ];
  instructions = "Keep changes focused and explain validation results.";
};
```

Package strings and objects (`source`, `autoload`, and Pi resource filters) are
supported. `settings.packages` can replace the bundled list; `packages` still
appends to it. Extensions and skills also append to their raw settings arrays.
Enabled package contributions participate in the default subagent extension
list; `autoload = false` packages do not. Standalone extensions are only shared
when explicitly listed in `subagentExtensions`. Raw
`settings.subagents.defaultExtensions` remains an explicit replacement.
Instructions append to the bundled guidance. Contributions use normal module
ordering; use `lib.mkBefore` or `lib.mkAfter` when order matters. Pi-disabled
profiles ignore these contributions. Use absolute or `~` paths; relative resources resolve from
`~/.pi/agent`, not the Nix module directory.

MCP servers are imported from Codex at activation and overridden by
`siraben.pi.mcpServers`. Set `siraben.pi.importCodexMcp = false` to disable import.
Generated `mcp.json` is a mutable, mode-0600 file outside the Nix store.
Keep secret literals out of Nix options; use runtime environment references or
Pi credential storage. OAuth, sessions and caches are unmanaged.

Extension sources are pinned in the root [flake.nix](../../../../flake.nix).
Build definitions and npm locks live in [packages/](packages/). GitHub inputs
advance with flake updates; versioned npm URLs need explicit release updates,
with matching dependency locks and `npmDepsHash` where used.
