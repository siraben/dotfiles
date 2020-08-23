{ config, pkgs, lib, currentSystem, ... }:

let
  inherit (builtins) currentSystem;
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  linuxPackages =  with pkgs; [
    smlnj
  ];
  darwinPackages = with pkgs; [
    smlnjBootstrap
  ];
  sharedPackages = with pkgs; [
    ag
    bat
    borgbackup
    cabal-install
    chez
    docker
    docker-compose
    ghc
    guile
    haskellPackages.haskell-language-server
    htop
    jq
    mpv
    mu
    nodePackages.bash-language-server
    nodePackages.node2nix
    nodePackages.npm
    nodejs
    python37Packages.pillow
    python37Packages.ueberzug
    ranger
    rustup
    texlive.combined.scheme-small
    the-powder-toy
    tldr
    tmux
    tor
    vim
    watch
    youtube-dl
    zathura
  ];
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "siraben";
  home.homeDirectory = "/Users/siraben";
  home.packages = sharedPackages
                  ++ (lib.optionals isLinux linuxPackages)
                  ++ (lib.optionals isDarwin darwinPackages);
  
  programs = {
    git.enable = true;
    emacs.enable = true;
  };

  home.stateVersion = "20.09";
}
