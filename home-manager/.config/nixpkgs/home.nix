{ config, pkgs, lib, currentSystem, ... }:

let
  inherit (builtins) currentSystem;
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  linuxPackages =  with pkgs; [
    anki
    feh
    firefox
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
  darwinPackages = with pkgs; [
    coreutils
    smlnjBootstrap
    emacsMacport
  ];
  sharedPackages = with pkgs; [
    ag
    agda
    alacritty
    bat
    borgbackup
    cabal-install
    chez
    cmake
    coq
    coqPackages.mathcomp
    docker
    docker-compose
    gcc
    gforth
    ghc
    guile
    haskellPackages.haskell-language-server
    htop
    idris
    jq
    kitty
    mpv
    mu
    nodePackages.bash-language-server
    nodePackages.npm
    nodejs
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
  ];
  darwinShellExtra = ''
    source $HOME/.nix-profile/etc/profile.d/nix.sh
  '';
  linuxShellExtra = '''';
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/siraben" else "/home/siraben";
  home.packages = sharedPackages
                  ++ (lib.optionals isLinux linuxPackages)
                  ++ (lib.optionals isDarwin darwinPackages);
  
  programs = {
    git = {
      enable = true;
      userName = "Ben Siraphob";
      userEmail = "bensiraphob@gmail.com";
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
