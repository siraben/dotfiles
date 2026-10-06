# siraben's dotfiles

macOS and Linux configuration using [Nix](https://nixos.org/) and
[Home Manager](https://github.com/nix-community/home-manager).

## Install

[Install Nix](https://nixos.org/download/), then:

```sh
git clone git@github.com:siraben/dotfiles.git ~/dotfiles
cd ~/dotfiles
./switch.sh
```

On Linux, select a profile with `./switch.sh minimal`, `headless`, or `full`.
Defaults are `full` on Apple Silicon/macOS and x86_64 Linux, and `headless` on
ARM64 Linux.

| Profile | Includes | Platforms |
| --- | --- | --- |
| `minimal` | Essential shell tools | x86_64/ARM64 Linux |
| `headless` | Minimal + Pi, Codex and other CLI development tools | x86_64/ARM64 Linux |
| `full` | Headless + Emacs, GUI and development tools | Apple Silicon/macOS, x86_64 Linux |

Flake outputs use `homeConfigurations."siraben@{arch}-{os}-{profile}"`, with
`aarch64-darwin`, `x86_64-linux`, or `aarch64-linux` as the platform.

## Customize

Import `homeManagerModules.default` (full), `.headless`, or `.minimal` into your
own configuration, or call `lib.mkHomeConfiguration` with `system`, `profile`,
`username` and `extraModules`. See [flake.nix](flake.nix) for these entry points
and [Pi configuration](home-manager/.config/nixpkgs/pi/README.md) for agent settings
and package updates. Emacs also uses externally installed fonts and language servers.
