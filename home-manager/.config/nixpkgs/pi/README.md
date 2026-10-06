# Pi configuration

[modules/](modules/) configures upstream Home Manager `programs.pi-coding-agent`.
Use `siraben.pi.settings` for recursive settings overrides, `siraben.pi.providers`
for models, and `siraben.pi.enableCuaDriver` to enable Cua on macOS. Pi is off by
default in minimal profiles. Companion files require the default `~/.pi/agent`
directory; settings, models and instructions are read-only.

MCP servers are imported from Codex at activation and overridden by
`siraben.pi.mcpServers`. Set `siraben.pi.importCodexMcp = false` to disable import.
Generated `mcp.json` is a mutable, mode-0600 file outside the Nix store.
Keep secret literals out of Nix options; use runtime environment references or
Pi credential storage. OAuth, sessions and caches are unmanaged.

Extension sources are pinned in the root [flake.nix](../../../../flake.nix).
Build definitions and npm locks live in [packages/](packages/). GitHub inputs
advance with flake updates; versioned npm URLs need explicit release updates,
with matching dependency locks and `npmDepsHash` where used.
