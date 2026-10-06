# Pi configuration

[modules/](modules/) configures upstream Home Manager `programs.pi-coding-agent`.
Use `siraben.pi.settings` for recursive settings overrides, `siraben.pi.providers`
for models, and `siraben.pi.enableCuaDriver` to enable Cua on macOS. Pi is off by
default in minimal profiles. Companion files require the default `~/.pi/agent`
directory; settings, models and instructions are read-only.

Home Manager owns `mcp.json` as a read-only file containing explicit
`siraben.pi.mcpServers` declarations and optional Cua configuration. Codex servers
are not imported. Activation replaces the previous mutable file: move desired
local declarations into Nix first and remove any `siraben.pi.importCodexMcp`
setting. Use runtime environment references or Pi credential storage for secrets,
never literal secrets in Nix options. OAuth, sessions and caches are unmanaged.

Extension sources are pinned in the root [flake.nix](../../../../flake.nix).
Build definitions and npm locks live in [packages/](packages/). GitHub inputs
advance with flake updates; versioned npm URLs need explicit release updates,
with matching dependency locks and `npmDepsHash` where used.
