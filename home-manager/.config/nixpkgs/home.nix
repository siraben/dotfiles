{ config, pkgs, lib, currentSystem, ... }:

let
  inherit (builtins) currentSystem;
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  linuxPackages =  with pkgs; [
    anki
    brave
    discord
    evince
    feh
    firefox
    gnome3.cheese
    gnumake
    keepassxc
    libreoffice
    msmtp
    musescore
    nextcloud-client
    offlineimap
    rhythmbox
    rofi
    slack
    smlnj
    spotify
    thunderbird
    tor-browser-bundle-bin
    transmission-gtk
    vlc
    whois
    zoom-us
  ];
  emacsGcc = (import (fetchTarball "https://github.com/twlz0ne/nix-gccemacs-darwin/archive/master.tar.gz")).emacsGccDarwin;
  darwinPackages = with pkgs; [
    coreutils
    emacsGcc
    smlnjBootstrap
  ];
  sharedPackages = with pkgs; [
    ag
    alacritty
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    bat
    borgbackup
    cabal-install
    cachix
    ccls
    chez
    clang
    coq
    docker
    docker-compose
    gforth
    ghc
    git
    guile
    haskellPackages.haskell-language-server
    htop
    kitty
    ledger
    mpv
    mu
    nnn
    nodePackages.bash-language-server
    nodePackages.javascript-typescript-langserver
    nodePackages.pyright
    python3
    python38Packages.pygments
    ranger
    rustup
    stow
    texlive.combined.scheme-medium
    the-powder-toy
    tldr
    tmux
    tor
    tree
    unzip
    vim
    w3m
    watch
    youtube-dl
    zathura
    zip
  ];
  darwinShellExtra = ''
    source $HOME/.nix-profile/etc/profile.d/nix.sh
  '';
  linuxShellExtra = '''';
in
{
  programs.home-manager.enable = true;

  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/siraben" else "/home/siraben";
  home.packages = sharedPackages
                  ++ (lib.optionals isLinux linuxPackages)
                  ++ (lib.optionals isDarwin darwinPackages);

  home.sessionVariables = {
    EDITOR = "vim";
  };

  programs = {
    git = {
      enable = true;
      userName = "Ben Siraphob";
      userEmail = "bensiraphob@gmail.com";
      extraConfig = {
        pull.rebase = true;
        github.user = "siraben";
      };
    };
    emacs.enable = isLinux;
    zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        theme = "robbyrussell";
      };
      history = {
        size = 100000;
        save = 100000;
        extended = true;
      };
      initExtra = if isDarwin then darwinShellExtra else linuxShellExtra;
    };
  };

  programs.direnv.enable = true;
  programs.direnv.enableNixDirenvIntegration = true;

  home.stateVersion = "20.09";
}
