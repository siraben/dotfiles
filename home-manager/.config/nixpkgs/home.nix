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
      (import ./overlay.nix)
    ];
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
  pkgs = import sources.nixpkgs pkgsOptions;
  masterPkgs = import sources.masterPkgs pkgsOptions;
  x86-darwin-pkgs = import sources.nixpkgs (pkgsOptions // { system = (if isDarwin then "x86_64-darwin" else builtins.currentSystem); });
  grammars = (pkgs.tree-sitter.override (with pkgs; {
    extraGrammars = {
      tree-sitter-promela = { src = sources.tree-sitter-promela.outPath; };
      tree-sitter-formula = { src = sources.tree-sitter-formula.outPath; };
      tree-sitter-sml = { src = sources.tree-sitter-sml.outPath; };
      tree-sitter-solidity = { src = sources.tree-sitter-solidity.outPath; };
    };
  })).builtGrammars;
  minimal = false;
in
lib.recursiveUpdate ({
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/siraben" else "/home/siraben";
  home.packages = import ./packages.nix { inherit lib sources pkgs x86-darwin-pkgs masterPkgs isDarwin isLinux minimal; };

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
})
(lib.optionalAttrs (!minimal) {
    home.file.".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: ''
          name=${name}
          ln -s ${src}/parser $out/bin/''${name#tree-sitter-}.so
        '')
        grammars)}
  '');
})
