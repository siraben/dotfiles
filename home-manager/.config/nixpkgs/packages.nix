{ lib, sources, pkgs, x86-darwin-pkgs, isDarwin, isLinux, minimal }:
let
  whenNotMinimal = lib.optionals (!minimal);
  gccemacs = with pkgs; (emacsPackagesFor emacsNativeComp).emacsWithPackages (e: [ e.vterm ]);
  nix-bisect = import sources.nix-bisect { inherit pkgs; };
  wrapWeb = pkgs.callPackage ./wrapWeb.nix { };
  wayland-packages = whenNotMinimal (with pkgs; [
    gccemacs
    firefox-wayland
  ]);
  web-shortcuts = whenNotMinimal (with pkgs; [
    (wrapWeb "element" "https://app.element.io")
    (wrapWeb "hn" "https://news.ycombinator.com")
    (wrapWeb "neverssl" "http://neverssl.com")
    (wrapWeb "mastodon" "https://mastodon.social")
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
  ]) ++ wayland-packages ++ web-shortcuts;
  darwinPackages = with pkgs; [
    coreutils
    gnused
    gccemacs
    spoof-mac
    icdiff
  ];
  # Package set to use when wanting to use x86-darwin packages on
  # aarch64-darwin.
  pkgs' = if isDarwin then x86-darwin-pkgs else pkgs;
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
    # haskellPackages.haskell-language-server
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodePackages.pyright
    rnix-lsp
  ];
  sharedPackages = with pkgs; [
    bash
    borgbackup
    curl
    htop
    vim
    watch
    wget
    mosh
  ] ++ (whenNotMinimal ([
    netcat-gnu
    pkgs'.agda
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    bat
    btop
    cabal-install
    cachix
    clang_14
    cmake
    dejavu_fonts
    emscripten
    ffmpeg
    github-cli
    gnumake
    google-cloud-sdk
    guile
    graphviz
    (import ./haskell-packages.nix { inherit pkgs; })
    hlint
    jq
    ledger
    mpv
    nixpkgs-review
    niv
    nmap
    nodePackages.typescript
    nodejs
    (import ./python-packages.nix { pkgs = pkgs'; })
    ranger
    ripgrep
    pkgs'.rmview
    rustup
    rust-analyzer
    shellcheck
    stow
    swiProlog
    (import ./texlive-packages.nix { inherit pkgs; })
    pkgs'.the-powder-toy
    tldr
    tor
    torsocks
    tree
    (tree-sitter.overrideAttrs (oA: { webUISupport = true; }))
    unzip
    yt-dlp
    zathura
    zip
  ] ++ coqPackages ++ languageServers));
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)
