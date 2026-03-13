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
- Custom package sets for
  - [Haskell](./home-manager/.config/nixpkgs/haskell-packages.nix)
  - [Python](./home-manager/.config/nixpkgs/python-packages.nix)

## Profiles

Home configurations use `{arch}-{os}-{profile}` triple naming:

| Profile    | Description                          | Packages                                                  |
|------------|--------------------------------------|-----------------------------------------------------------|
| `minimal`  | Bare essentials                      | bash, curl, htop, vim, wget, mosh, gh, ranger, croc, etc. |
| `headless` | CLI tools for servers                | minimal + claude-code, codex, bat, ripgrep, jq, etc.      |
| `full`     | Everything including GUI and dev     | headless + emacs, kitty, firefox, haskell, rust, etc.      |

Available configurations:

```
siraben@x86_64-darwin-full
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
$ ./switch.sh full
```

## NixOS Configurations

| Host         | Arch           | Description                  |
|--------------|----------------|------------------------------|
| `beelink`    | x86_64-linux   | Beelink Mini S12 Pro desktop |
| `server`     | x86_64-linux   | x86_64 server                |
| `server-arm` | aarch64-linux  | OCI ARM instance             |

## Notes
Some configuration (e.g. Emacs) has deliberately not been Nixified so that it works independently. For some things like Emacs it assumes you have installed external dependencies such as fonts, interpreters and language servers for various programming languages.
