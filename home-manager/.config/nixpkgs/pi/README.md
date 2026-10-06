# Pi configuration

`../base.nix` imports `modules/` and adds `overlay.nix` after the official Pi
overlay. Package names and compatibility entry points are preserved.

## Layout

| Path | Responsibility |
| --- | --- |
| `modules/default.nix` | Option declarations and module imports |
| `modules/settings.nix` | Package selection, defaults, providers, theme, and subagent extension list |
| `modules/mcp.nix` | Explicit MCP declarations in a managed JSON file |
| `modules/cua-driver.nix` | Optional macOS package, environment, and launchd service |
| `modules/instructions.nix` | Managed background-work guidance |
| `modules/extensions.nix` | Installation of local TypeScript extensions |
| `packages/` | One derivation per extension, plus optional Cua Driver |
| `themes/` | Managed theme asset |
| `cua-environment.nix` | Environment shared by the Cua service and MCP declaration |
| `overlay.nix` | Small package assembly layer |

The original `../pi.nix`, `../pi-home.nix`, and `../cua-driver.nix` paths remain
compatibility entry points. Local TypeScript sources/tests remain at their
existing paths; their contents are unchanged.

## Source inputs and updates

All external Pi extension sources are declared in the root flake:

| Input | Package | Update behavior |
| --- | --- | --- |
| `pi-subagents` | pi-subagents | GitHub source, pinned by `flake.lock` |
| `pi-better-harness` | pi-better-background-tasks | Monorepo subdirectory, pinned by `flake.lock` |
| `kendex` | pi-tool-renderer | Monorepo subdirectory, pinned by `flake.lock` |
| `pi-tool-summaries` | pi-tool-summaries | GitHub source, pinned by `flake.lock` |
| `pi-codex-goal` | pi-codex-goal | Published npm release tarball |
| `pi-web-access` | pi-web-access | Published npm release tarball |
| `context-mode` | context-mode | Published npm release tarball |

The npm inputs deliberately retain the existing releases and generated bundles.
A plain `nix flake update` (including the scheduled lockfile updater) advances
unpinned GitHub inputs, but does **not** select a newer versioned npm URL. To
upgrade an npm release, change its root input URL and update its lock entry.
For web access and context-mode, also regenerate the corresponding
`packages/*-package-lock.json` with the existing production/legacy-peer-deps
policy and update `npmDepsHash` in the package file. Do not substitute GitHub
source for a published bundle without adding and validating its build steps.
Flake source hashes and npm dependency hashes serve different purposes.

Versions come from input package metadata where available. The tool-summary
package retains its existing unstable version label for this reorganization.
The subagents build still uses its upstream lock and the conditional Pi API
adapter; no compatibility workaround or dependency hash is dropped.
Cua Driver is a separate signed macOS application, not a Pi extension, and keeps
its existing fixed release derivation.

## Behavior and validation

Pi remains disabled by default for `minimal`. As before, the two local extension
files are installed even in that profile. Settings recursively merge user
options; explicit MCP entries override the default disabled `computer-use` stub
and optional Cua declaration. Cua Driver remains opt-in on macOS.

Home Manager owns `~/.pi/agent/mcp.json` as a forced, read-only store link.
Only `siraben.pi.mcpServers` and the built-in declarations populate it. Codex's
configuration is not read or synchronized. The `siraben.pi.importCodexMcp`
option and importer checks have been removed; remove that option from callers.
On the next activation the generated file replaces the previous mutable MCP
file, so move any desired server declarations into `siraben.pi.mcpServers`
first. Local edits to `mcp.json` are not preserved. Never put secret literals
in these Nix declarations: use runtime environment references such as
`${TOKEN}` or Pi's separate runtime credential storage. OAuth, sessions and
caches remain unmanaged and outside the Nix store.

Build the Linux headless Home Manager activation package without switching:

```bash
nix build '.#homeConfigurations."siraben@x86_64-linux-headless".activationPackage' --no-link
```

Also evaluate the exported full/minimal and ARM configurations after changes.

## Upstream Home Manager integration

The pinned Home Manager (`acd21c5a3420a9d5fd0ed06299b10828267ef9ba`)
already provides `programs.pi-coding-agent`; this migration requires no input
upgrade. Its module owns package installation, JSON generation, and global
context installation. `package = pkgs.pi` retains the official Pi overlay
package instead of selecting Home Manager's default package.

| Feature | Owner after migration | Compatibility |
| --- | --- | --- |
| Pi executable | Upstream `package` | Same derivation/version |
| Defaults and recursive overrides | `siraben.pi.settings` → upstream `settings` | Same values; lists still replace through the compatibility option |
| Providers | `siraben.pi.providers` → upstream `models.providers` | Same JSON, including empty providers |
| Global guidance | Upstream `context` | Same AGENTS.md bytes |
| Keybindings/additional system guidance | Upstream `keybindings` / `appendSystem` | Newly accessible; absent by default |
| Extra executable dependencies | Upstream `extraPackages` | Optional PATH wrapper; not used by defaults |
| Extension packages and context-mode | Existing Nix derivations and native package paths | Same npm closures, adapter logic, autoload flags and subagent list |
| Local extensions/theme | Companion `home.file` entries | Same targets; extensions remain present in minimal profiles |
| MCP declarations | Companion `home.file` | Explicit declarations, disabled stub and optional Cua server; no Codex import |
| Credentials and mutable state | Pi/runtime home | Runtime credentials, OAuth, sessions and caches remain unmanaged; managed MCP JSON must contain no secret literals |
| Cua Driver | Companion Darwin module | Same opt-in package, environment and launchd service |
| Profiles and exported modules | Existing entry points | Same enable defaults and platform conditions |

Settings, models, and instructions remain forced, immutable Home Manager links.
Upstream JSON formatting/store names change, but decoded JSON and destination
paths do not. MCP JSON is also a forced, immutable link. OAuth, sessions, and caches are
not managed by these modules. As before, declarative settings/provider/MCP
options are public Nix data: use environment references or Pi runtime credential
storage instead of putting secret literals in those options.

Use `siraben.pi.settings` for existing overrides. Direct upstream settings use
normal Home Manager merging (including list concatenation and scalar conflicts);
use `lib.mkForce` for a conflicting upstream value. The companion resources and
MCP file currently require the default `~/.pi/agent` directory; an assertion rejects
an upstream `configDir` override rather than allowing mismatched locations.
Set `siraben.pi.enable = false` to disable the integrated configuration.

The pinned Pi (`cd32f7725fdbddbaecdff5b1e68491563394e0ca`) documents native
`mcp.json`, environment expansion, dynamic extension-registered servers and
local package paths. This migration does not add/remove an MCP adapter or change
MCP semantics. Extension source inputs still do not replace npm dependency
closures.

### Migration validation

Compared the parent commit `b0b5211` with the migration across all six exported
Home Manager configurations and an additional Darwin configuration enabling
Cua Driver, replacing the package list, overriding a nested setting, and adding
a model provider and MCP declaration. Compare decoded settings/models, global
instruction text, resource source paths/targets/force flags, installed package
sets, session environment, MCP configuration, and launchd configuration.

Platform builds and regression results are recorded in PR #88. Builds do not
activate a generation or touch running Pi sessions. A live authenticated MCP/Cua
connection and interactive Pi session are outside this non-activation validation.
