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

Intel macOS and ARM Linux full profiles are not exported.

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

Pi can be extended without replacing its managed files: `siraben.pi.settings`
recursively overlays `settings.json`, while `siraben.pi.providers` and
`siraben.pi.mcpServers` populate `models.json` and `mcp.json` respectively.

## NixOS Configurations

| Host         | Arch           | Description                  |
|--------------|----------------|------------------------------|
| `server`     | x86_64-linux   | x86_64 server                |

## Notes

Some configuration (e.g. Emacs) has deliberately not been Nixified so that it works independently. For some things like Emacs it assumes you have installed external dependencies such as fonts, interpreters and language servers for various programming languages.
