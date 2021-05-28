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
  pkgs = import sources.nixpkgs {
    overlays = [
      (import sources.nixpkgs-wayland)
      (import sources.emacs-overlay)
    ];
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
in
{
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/siraben" else "/home/siraben";
  home.packages = import ./packages.nix { inherit lib sources pkgs isDarwin isLinux; };

  home.sessionVariables = {
    EDITOR = "emacsclient";
  } // (lib.optionalAttrs isLinux {
    XDG_CURRENT_DESKTOP = "sway";
  });

  programs = import ./programs.nix { inherit lib pkgs isDarwin isLinux; };
  services = import ./services.nix { inherit lib pkgs isDarwin isLinux; };
  home.stateVersion = "20.09";
}
