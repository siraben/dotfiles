# siraben's dotfiles

Configuration for my macOS and Linux systems using
[Nix](https://nixos.org/) and [Home
Manager](https://github.com/nix-community/home-manager). I mostly use
macOS now, Linux minimal in server environments.

## Summary
- OS: NixOS and macOS
- Package manager: Nix
- Shell: `zsh` with [pure prompt](https://github.com/sindresorhus/pure)
- WM on NixOS: wayland
- Filesystem: ZFS on NixOS, APFS on macOS
- Editor: Emacs, `tomorrow-night` theme, [straight.el](https://github.com/raxod502/straight.el)
- Custom package sets for
  - [LaTeX](./home-manager/.config/nixpkgs/texlive-packages.nix)
  - [Haskell](./home-manager/.config/nixpkgs/haskell-packages.nix)
  - [Python](./home-manager/.config/nixpkgs/python-packages.nix)

## Installation
[Install Nix](https://nixos.org/download/) on macOS or Linux, then run the following commands.

```shell-session
$ git clone git@github.com:siraben/dotfiles.git ~/dotfiles
$ cd ~/dotfiles && ./switch.sh
```

## Notes
Some configuration (e.g. Emacs) has deliberately not been Nixified so that it works independently. For some things like Emacs it assumes you have installed external dependencies such as fonts, interpreters and language servers for various programming languages.
