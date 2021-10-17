# siraben's dotfiles

Configuration for my macOS and Linux systems using [Nix](https://nixos.org/) and [Home Manager](https://github.com/nix-community/home-manager).

In particular, the build of Emacs is custom and is [built with GitHub Actions](https://github.com/siraben/nix-gccemacs-darwin/actions) from source from the Emacs source repository.  This is because I want to take advantage of things like native compilation of Elisp and use [libvterm](https://github.com/akermu/emacs-libvterm).

## Summary
- OS: NixOS and macOS
- Package manager: Nix (duh) (version 2.4)
- Shell: `zsh` with [pure prompt](https://github.com/sindresorhus/pure)
- WM on NixOS: GNOME 40
- Filesystem: ZFS on NixOS, APFS on macOS
- Editor: Emacs (bleeding edge), `tomorrow-night` theme

## Installation
First, [install Home Manager](https://github.com/nix-community/home-manager#installation) on macOS or Linux, then run the following commands.

```shell-session
$ git clone git@github.com:siraben/dotfiles.git
$ cd dotfiles && stow home-manager
$ home-manager switch
```

## Notes
Some configuration (e.g. Emacs, i3) has deliberately not been Nixified so that it works independently.  In general, every folder except for `nixos` can be `stow`'d.  Note that for some things like Emacs it assumes you have installed external dependencies such as fonts, interpreters and language servers for various programming languages.
