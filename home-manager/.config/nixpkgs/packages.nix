{ lib, sources, pkgs, x86-darwin-pkgs, isDarwin, isLinux, minimal }:
let
  whenNotMinimal = lib.optionals (!minimal);
  gccemacs = with pkgs; (emacsPackagesFor emacs29).emacsWithPackages (e: [ e.vterm ]);
  wrapWeb = pkgs.callPackage ./wrapWeb.nix { };
  wayland-packages = whenNotMinimal (with pkgs; [
    gccemacs
    firefox-wayland
  ]);
  linuxPackages = whenNotMinimal (with pkgs; [
    discord
    docker
    docker-compose
    evince
    gnome3.cheese
    keepassxc
    killall
    kitty
    libreoffice
    msmtp
    musescore
    nextcloud-client
    nix-update
    offlineimap
    paper-icon-theme
    racket
    rhythmbox
    slack
    spotify
    tdesktop
    thunderbird
    tor-browser-bundle-bin
    transmission-gtk
    ungoogled-chromium
    vlc
    whois
    zoom-us
  ]) ++ wayland-packages;
  darwinPackages = with pkgs; [
    coreutils
    gnused
    gccemacs
    spoof-mac
    pinentry_mac
  ];
  coqPackages = with pkgs; [
    coqPackages_8_13.hierarchy-builder
    coqPackages_8_13.QuickChick
    coqPackages_8_13.mathcomp
    coqPackages_8_13.coqhammer
    coqPackages_8_13.mathcomp-ssreflect
    coq_8_13
    # External provers for coq-hammer
    eprover
    vampire
    z3-tptp
    (if stdenv.isDarwin then
      ((import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/a3e1e9271e0ff87309d44f9817baadb09b305757.tar.gz";
      }) {}).cvc4)
    else cvc4)
  ];
  languageServers = with pkgs; [
    haskellPackages.haskell-language-server
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    ruff-lsp
  ];
  sharedPackages = with pkgs; [
    bash
    curl
    htop
    vim
    watch
    wget
    mosh
  ] ++ (whenNotMinimal ([
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    bat
    btop
    borgbackup
    cabal-install
    cachix
    cmake
    dejavu_fonts
    ffmpeg
    github-cli
    gnumake
    guile
    graphviz
    (import ./haskell-packages.nix { inherit pkgs; })
    hlint
    imagemagick
    jq
    ledger
    mpv
    nixpkgs-review
    niv
    nmap
    nodejs
    (import ./python-packages.nix { inherit pkgs; })
    ranger
    ripgrep
    rust-analyzer
    shellcheck
    stow
    (import ./texlive-packages.nix { inherit pkgs; })
    the-powder-toy
    tldr
    tree
    (tree-sitter.overrideAttrs (oA: { webUISupport = true; }))
    typst
    unzip
    yt-dlp
    zathura
    zip
  ] ++ coqPackages ++ languageServers));
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)
