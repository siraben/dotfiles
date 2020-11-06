{ config, lib, currentSystem, ... }:

let
  pkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2020-11-01";
    url =
      "https://github.com/NixOS/nixpkgs/archive/26d3fbf21563c52f124551eb60dec5ea17ade8fa.tar.gz";
    sha256 = "08xhvfnwy60yyyhkddg7lknm9g7w2pg6lkyha5kfkg98c09f1p10";
  }) { };
  inherit (builtins) currentSystem;
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  comma = import (pkgs.fetchFromGitHub {
    owner = "Shopify";
    repo = "comma";
    rev = "4a62ec17e20ce0e738a8e5126b4298a73903b468";
    sha256 = "0n5a3rnv9qnnsrl76kpi6dmaxmwj1mpdd2g0b4n1wfimqfaz6gi1";
  }) { };
  cargo2nix = (import (pkgs.fetchFromGitHub {
    owner = "tenx-tech";
    repo = "cargo2nix";
    rev = "a0f38b977596c39f4127b67c84a6930b3cbd662a";
    sha256 = "0dq2nc7n4clvxm1592dr1s8d4gqy0pq6z1xlxy1dfmf18hij4k6d";
  }) { }).package;
  haskell-hls = (import (pkgs.fetchFromGitHub {
    owner = "siraben";
    repo = "nix-haskell-hls";
    rev = "06b44e936b604f9ffeb1c6e3a444d8587c9f0cec";
    sha256 = "0v63saspagqjhi0dihiblw1z7xhm1pi7sw7zdg885pykjs9nbbq3";
  }) { unstable = true; });
  linuxPackages = with pkgs; [
    anki
    arc-theme
    brave
    discord
    dmenu
    docker
    docker-compose
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
    paper-icon-theme
    poppler_utils
    rhythmbox
    rofi
    scrot
    slack
    spotify
    system-config-printer
    thunderbird
    tor-browser-bundle-bin
    transmission-gtk
    vlc
    whois
    wpa_supplicant
    xorg.xkill
    zoom-us
  ];
  gccemacs = (import (pkgs.fetchFromGitHub {
    owner = "twlz0ne";
    repo = "nix-gccemacs-darwin";
    rev = "c10323fd2253d7b73bd6e06dd2cead5b3231df52";
    sha256 = "0bpm5isv687lf13lhi9ad6zaj820g5144293c114gxxlhl32f1wh";
  })).emacsGccDarwin;
  darwinPackages = with pkgs; [ coreutils gccemacs ];
  sharedPackages = with pkgs; [
    ag
    agda
    alacritty
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    autojump
    bat
    borgbackup
    cabal-install
    cachix
    cargo2nix
    ccls
    chez
    clang-tools
    clang_9
    cmake
    comma
    coq
    ghc
    git
    github-cli
    guile
    haskell-hls.hls
    haskell-hls.hls-wrapper
    haskellPackages.ghcide
    hlint
    htop
    jq
    kitty
    ledger
    mpv
    mu
    niv
    nixfmt
    nixpkgs-fmt
    nodePackages.bash-language-server
    nodePackages.javascript-typescript-langserver
    nodePackages.pyright
    nodejs
    python3
    ranger
    ripgrep
    rustup
    shellcheck
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
  linuxShellExtra = "";
  sharedShellExtra = "";
in {
  programs.home-manager.enable = true;

  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/siraben" else "/home/siraben";
  home.packages = sharedPackages ++ (lib.optionals isLinux linuxPackages)
    ++ (lib.optionals isDarwin darwinPackages);

  home.sessionVariables = { EDITOR = "emacsclient"; };

  programs = {
    broot.enable = true;
    emacs.enable = isLinux;
    git = {
      enable = true;
      userName = "Ben Siraphob";
      userEmail = "bensiraphob@gmail.com";
      extraConfig = {
        pull.rebase = true;
        github.user = "siraben";
      };
    };
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      oh-my-zsh = {
        enable = true;
        theme = "robbyrussell";
        plugins = [ "git" "autojump" ];
      };
      history = {
        size = 100000;
        save = 100000;
        extended = true;
      };
      shellAliases = {
        hm = "home-manager";
        nb = "nix build";
        nc = "nix channel";
        ncg = "nix-collect-garbage";
        ne = "nix edit";
        nr = "nix repl";
        ns = "nix-shell";
      };
      initExtra = lib.concatStringsSep "\n"
        [ (lib.optionalString isDarwin darwinShellExtra)
          (lib.optionalString isLinux linuxShellExtra)
          sharedShellExtra
        ];
    };
  };

  programs.direnv.enable = true;
  programs.direnv.enableNixDirenvIntegration = true;

  home.stateVersion = "20.09";
}
