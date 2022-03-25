{ config, lib, currentSystem, ... }:
# To put pkgs in scope in nix repl
# :a (import (import nix/sources.nix).nixpkgs) {}
let
  inherit (lib.systems.elaborate { system = builtins.currentSystem; }) isLinux isDarwin;
  sources = import ../../../nix/sources.nix;
  unfreePackages = [
    "discord"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "zoom"
  ];
  pkgsOptions = {
    overlays = [
      (import sources.emacs-overlay)
    ] ++ lib.optional isLinux (import sources.nixpkgs-wayland);
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
  pkgs = import sources.nixpkgs pkgsOptions;
  pkgsStable = import sources.pkgsStable pkgsOptions;
  x86-darwin-pkgs = import sources.nixpkgs (pkgsOptions // { system = (if isDarwin then "x86_64-darwin" else builtins.currentSystem); });
  grammars = (pkgs.tree-sitter.override (with pkgs; {
    extraGrammars = {
      tree-sitter-promela = {
        src = fetchFromGitHub {
          repo = "tree-sitter-promela";
          owner = "siraben";
          rev = "91da8f141c3c4c695eb71018c8a7b2e7ea39c167";
          sha256 = "sha256-JK7+ZfR6uHdhDlnVJLwNtu5UbruClaIqlaRREG1iVG0=";
        };
      };
      tree-sitter-formula = {
        src = fetchFromGitHub {
          repo = "tree-sitter-formula";
          owner = "siraben";
          rev = "61f4741d5aad6e21fcb20412583b239fbcf28b4c";
          sha256 = "sha256-hPpeDjMnQwTOfxII2KGHqMN1GlPLCB5hjJPj3dkidxA=";
        };
      };
    };
  })).builtGrammars;

in
{
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/siraben" else "/home/siraben";
  home.packages = import ./packages.nix { inherit lib sources pkgs pkgsStable x86-darwin-pkgs isDarwin isLinux; };
  home.file.".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: ''
          name=${name}
          ln -s ${src}/parser $out/bin/''${name#tree-sitter-}.so
        '')
        grammars)}
  '');

  home.sessionVariables = {
    EDITOR = "emacsclient";
    COQPATH="$HOME/.nix-profile/lib/coq/8.13/user-contrib";
  } // (lib.optionalAttrs isLinux {
    XDG_CURRENT_DESKTOP = "sway";
    MOZ_ENABLE_WAYLAND = 1;
  });

  home.language = {
    ctype = "en_US.UTF-8";
    base = "en_US.UTF-8";
  };

  programs = import ./programs.nix { inherit lib pkgs isDarwin isLinux; };
  fonts.fontconfig.enable = true;
  services = lib.optionalAttrs isLinux (import ./services.nix { inherit lib pkgs; });
  home.stateVersion = "21.11";
}
