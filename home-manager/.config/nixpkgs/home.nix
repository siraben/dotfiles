{ config, pkgs, lib, currentSystem, ... }:

let
  inherit (builtins) currentSystem;
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  comma = import (pkgs.fetchFromGitHub {
      owner = "Shopify";
      repo = "comma";
      rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
      sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
  }) {};
  cargo2nix = (import (pkgs.fetchFromGitHub {
    owner = "tenx-tech";
    repo = "cargo2nix";
    rev = "a0f38b977596c39f4127b67c84a6930b3cbd662a";
    sha256 = "0dq2nc7n4clvxm1592dr1s8d4gqy0pq6z1xlxy1dfmf18hij4k6d";
  }) {}).package;
  linuxPackages  = with pkgs; [
    anki
    arc-theme
    brave
    discord
    dmenu
    evince
    exfat
    feh
    firefox
    gnome3.cheese
    gnumake
    gnupg
    keepassxc
    killall
    libreoffice
    lightlocker
    msmtp
    musescore
    networkmanager
    nextcloud-client
    nitrogen
    offlineimap
    pandoc
    paper-icon-theme
    poppler_utils
    rhythmbox
    rofi
    scrot
    slack
    smlnj
    spotify
    system-config-printer
    thunderbird
    tor-browser-bundle-bin
    transmission-gtk
    vlc
    whois
    wpa_supplicant
    xorg.xkill
    xss-lock
    zoom-us
  ];
  gccemacs = (import (pkgs.fetchFromGitHub {
      owner = "twlz0ne";
      repo = "nix-gccemacs-darwin";
      rev = "c10323fd2253d7b73bd6e06dd2cead5b3231df52";
      sha256 = "0bpm5isv687lf13lhi9ad6zaj820g5144293c114gxxlhl32f1wh";
  })).emacsGccDarwin;
  darwinPackages = with pkgs; [
    coreutils
    gccemacs
    smlnjBootstrap
  ];
  sharedPackages = with pkgs; [
    ag
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    bat
    borgbackup
    cabal-install
    cachix
    cargo2nix
    ccls
    chez
    clang-tools
    clang_8
    cmake
    comma
    coq
    docker
    docker-compose
    gforth
    ghc
    git
    github-cli
    guile
    haskellPackages.haskell-language-server
    haskellPackages.ghcide
    htop
    jq
    kitty
    ledger
    mpv
    mu
    nnn
    nodejs
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
    EDITOR = "emacsclient";
    NNN_PLUG="p:-_less -iR $nnn*;l:-_git log";
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
