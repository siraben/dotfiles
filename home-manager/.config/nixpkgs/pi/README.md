# Pi configuration

Configure [modules/](modules/) through `siraben.pi.settings`, `.providers`, and
`.mcpServers`; enable macOS Cua with `.enableCuaDriver`. Pi is disabled in minimal
profiles and requires `~/.pi/agent`.

Home Manager owns read-only settings and `mcp.json`; Codex servers are not imported.
Before activation, move local MCP declarations into `.mcpServers` and remove
`importCodexMcp`. Keep secrets in runtime environment variables or Pi credential
storage, never Nix options. OAuth, sessions and caches remain unmanaged.

Update extension sources in [flake.nix](../../../../flake.nix) and builds in
[packages/](packages/). Versioned npm URLs require explicit updates alongside
dependency locks and `npmDepsHash`; flake updates alone do not upgrade them.
