# siraben's dotfiles

Configuration for my macOS and Linux systems using
[Nix](https://nixos.org/) and [Home
Manager](https://github.com/nix-community/home-manager).

## Summary

- OS: NixOS and macOS
- Package manager: Nix
- Shell: `zsh` with [pure prompt](https://github.com/sindresorhus/pure)
- WM on NixOS: wayland
- Editor: Emacs, `tomorrow-night` theme, [straight.el](https://github.com/raxod502/straight.el)
- Custom [Python environment](./home-manager/.config/nixpkgs/python-packages.nix)

## Profiles

Home configurations use `{arch}-{os}-{profile}` triple naming:

| Profile    | Description                          | Packages                                                  |
|------------|--------------------------------------|-----------------------------------------------------------|
| `minimal`  | Bare essentials                      | bash, curl, htop, vim, wget, mosh, gh, ranger, croc, etc. |
| `headless` | CLI tools for servers                | minimal + Pi, Agent Deck, Claude Code, Codex, bat, ripgrep, jq, etc.      |
| `full`     | Everything including GUI and dev     | headless + Emacs, Node.js, Python, Typst; Firefox/Kitty on Linux      |

Available configurations:

```
siraben@aarch64-darwin-full
siraben@x86_64-linux-full
siraben@x86_64-linux-headless
siraben@x86_64-linux-minimal
siraben@aarch64-linux-headless
siraben@aarch64-linux-minimal
```

## Installation

[Install Nix](https://nixos.org/download/) on macOS or Linux, then:

```shell-session
$ git clone git@github.com:siraben/dotfiles.git ~/dotfiles
$ cd ~/dotfiles && ./switch.sh
```

`switch.sh` auto-detects arch and OS. On Linux, pass a profile:

```shell-session
$ ./switch.sh              # default (full on x86_64, headless on aarch64)
$ ./switch.sh minimal
$ ./switch.sh headless
$ ./switch.sh full         # x86_64 Linux only
```

The full profile installs Emacs and GNU Stow. Link the tracked Emacs
configuration after the first switch:

```shell-session
$ cd ~/dotfiles && stow --target "$HOME" emacs
```

## Composition

The flake exports `homeManagerModules.default` and
`lib.mkHomeConfiguration` so a separate host or private flake can reuse the
public configuration without copying it. Pass private modules through
`extraModules` and keep the private flake's own lock file pinned:

```nix
{
  inputs.dotfiles.url = "github:siraben/dotfiles";

  outputs = { dotfiles, ... }: {
    homeConfigurations.work = dotfiles.lib.mkHomeConfiguration {
      system = "x86_64-linux";
      profile = "headless";
      username = "work-user";
      extraModules = [ ./work.nix ];
    };
  };
}
```

Pi can be extended without replacing its managed files. `siraben.pi.settings`
recursively overlays `settings.json`; typed package, extension, skill, prompt,
theme, instruction, provider, and MCP options compose additional resources.
Subagent extension sharing is explicit and credentials remain runtime
environment references rather than Nix values.

Modules can extend the unfree package allowlist without changing package
selection:

```nix
siraben.allowedUnfreePackages = [ "example-package" ];
```

Names use `lib.getName` without versions. Module values append to the default
allowlist; use `lib.mkForce [ ... ]` to replace it. This permits packages but
does not install them.

## Pi configuration

Pi comes from its upstream Nix flake. Home Manager modules and extension package
definitions live under [pi/](./home-manager/.config/nixpkgs/pi/README.md).
Extension sources use flake inputs or pinned npm release URLs and dependency
locks. CI evaluates all flake outputs, checks Pi package pins, and builds every
advertised Home Manager configuration. See the Pi directory's update notes
before changing releases.

## Notes

The Emacs configuration runs independently of Nix and expects its fonts,
interpreters, and language servers to be available on `PATH`.
