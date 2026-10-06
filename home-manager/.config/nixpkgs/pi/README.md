# Pi configuration

`../base.nix` imports `modules/` and adds `overlay.nix` after the official Pi
overlay. The public `siraben.pi` options and package names are unchanged.

## Layout

| Path | Responsibility |
| --- | --- |
| `modules/default.nix` | Option declarations and module imports |
| `modules/settings.nix` | Package selection, defaults, providers, theme, and subagent extension list |
| `modules/mcp.nix` | MCP declarations and activation wiring |
| `modules/write-mcp-config.py` | Codex conversion, merge order, private atomic JSON writes |
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
options; declared MCP entries override imported Codex fields; Cua Driver remains
opt-in on macOS. The existing disabled `computer-use` stub is preserved.

Run the extracted writer’s regression checks with Python 3.11 or newer
(the configured Nix Python includes `tomllib`), without touching the real home:

```bash
python3 -m unittest discover -s home-manager/.config/nixpkgs/pi/tests
```

The same tests are exposed as `checks.<system>.pi-mcp-config` for all supported
systems, so they also run as part of `nix flake check`. To run them independently:

```bash
nix build .#checks.x86_64-linux.pi-mcp-config --no-link
```

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
| MCP import and declarations | Companion activation writer | Same Codex conversion, shallow per-server override, disabled stub, runtime environment references |
| Credentials and mutable state | Pi/runtime home | Imported credentials remain outside store; MCP JSON remains atomic and mode 0600 |
| Cua Driver | Companion Darwin module | Same opt-in package, environment and launchd service |
| Profiles and exported modules | Existing entry points | Same enable defaults and platform conditions |

Settings, models, and instructions remain forced, immutable Home Manager links.
Upstream JSON formatting/store names change, but decoded JSON and destination
paths do not. MCP remains a mutable regular file. OAuth, sessions, and caches are
not managed by these modules. As before, declarative settings/provider/MCP
options are public Nix data: use environment references or Pi runtime credential
storage instead of putting secret literals in those options.

Use `siraben.pi.settings` for existing overrides. Direct upstream settings use
normal Home Manager merging (including list concatenation and scalar conflicts);
use `lib.mkForce` for a conflicting upstream value. The companion resources and
writer currently require the default `~/.pi/agent` directory; an assertion rejects
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
sets, session environment, MCP activation command, and launchd configuration.

Platform builds and regression results are recorded in PR #88. Builds do not
activate a generation or touch running Pi sessions. A live authenticated MCP/Cua
connection and interactive Pi session are outside this non-activation validation.
