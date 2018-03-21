# siraben's dotfiles
### By Siraphob (Ben) Phipathananunth

This is a collection of my dotfiles. These are highly tailored to my
tastes so don't expect me to merge drastically-altering pull requests!

## Supported platforms
Works on any Linux platform that allows for the installation of the
dependencies (e.g. through a package manager, or building from
source).

All versions of macOS that support Homebrew.

## Installation Guide
On macOS, run:
``` bash
source <(curl -fsSL https://raw.githubusercontent.com/siraben/dotfiles/master/setup-mac.sh)
```

## What is configured?
- Emacs
- Homebrew

## Dependencies
### Linux
- Emacs (>= 24.3)
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
- [ ] Lookup other dotfiles setups and learn from them
- [ ] Add `setup-linux.sh`

