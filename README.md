# siraben's dotfiles
### By Siraphob (Ben) Phipathananunth

This is a collection of my dotfiles.  The configurations are highly
tailored to my tastes, and may or may not be suited to yours.

## Supported platforms
Works on any GNU/Linux platform that allows for the installation of
the dependencies (e.g. through a package manager such as Apt, Nix or
Guix, or building from source).

All versions of macOS that support Homebrew.

## Installation Guide
On a macOS system, run:

``` bash
curl -fsSL https://raw.githubusercontent.com/siraben/dotfiles/master/setup-mac.sh | bash
```

## What is configured?
### Both Platforms
- Emacs
### macOS
- Homebrew
  - Packages
    - aspell
    - guile
  - Casks
    - iTerm 2
    - Firefox
    - GitHub Desktop
    - KeepassXC
    - VirtualBox
    - Nextcloud Desktop
    - f.lux
    - Thunderbird

## Dependencies
### GNU/Linux
- GNU Emacs (>= 26.1)
- GNU Stow

On macOS, the dependencies are automatically installed.

## To-do items
- [x] Add `setup-macos.sh`
  - [x] Automated setup of [Oh My Zsh!](https://ohmyz.sh)
  - [x] Homebrew
- [ ] Extend `setup-macos.sh`
  - [ ] Configure privacy settings
  - [ ] Firefox user.js
  - [ ] Run partially if certain things have been set up before
        (e.g. don't install homebrew again etc...)
  - [x] Configure key repeat
  - [x] Configure other `defaults write` commands
- [x] Add `setup-linux.sh`

