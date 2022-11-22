{ lib, sources, pkgs, x86-darwin-pkgs, masterPkgs, isDarwin, isLinux, minimal }:
let
  whenNotMinimal = lib.optionals (!minimal);
  # gccemacs = (import sources.nix-gccemacs-darwin).pkgs.aarch64-darwin.emacsGccDarwin;
  gccemacs = with pkgs; (emacsPackagesFor emacsNativeComp).emacsWithPackages (e: [ e.vterm ]);
  nix-bisect = import sources.nix-bisect { inherit pkgs; };
  # nixpkgs-review = import sources.nixpkgs-review { inherit pkgs; };
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
    clang_13
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
    cool-osx
    icdiff
  ];
  # Package set to use when wanting to use x86-darwin packages on
  # aarch64-darwin.
  pkgs' = if isDarwin then x86-darwin-pkgs else pkgs;
  coqPackages = with pkgs; [
    masterPkgs.coqPackages_8_13.hierarchy-builder
    masterPkgs.coqPackages_8_13.QuickChick
    masterPkgs.coqPackages_8_13.simple-io
    masterPkgs.coqPackages_8_13.coq-ext-lib
    masterPkgs.coqPackages_8_13.mathcomp
    masterPkgs.coqPackages_8_13.coqhammer
    masterPkgs.coqPackages_8_13.coq-elpi
    masterPkgs.coqPackages_8_13.trakt
    masterPkgs.coqPackages_8_13.stdpp
    masterPkgs.coqPackages_8_13.serapi
    masterPkgs.coqPackages_8_13.mathcomp-ssreflect
    masterPkgs.coq_8_13
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
    cmake
    dejavu_fonts
    emscripten
    ffmpeg
    github-cli
    gnumake
    guile
    graphviz
    (import ./haskell-packages.nix { inherit pkgs; })
    hlint
    jq
    ledger
    mpv
    niv
    nixpkgs-review
    nmap
    nodePackages.typescript
    nodejs
    (import ./python-packages.nix { pkgs = pkgs'; })
    ranger
    ripgrep
    pkgs'.rmview
    rustup
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
