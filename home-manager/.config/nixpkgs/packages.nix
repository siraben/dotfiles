{ lib, sources, pkgs, pkgsStable, x86-darwin-pkgs, isDarwin, isLinux }:
let
  gccemacs = if
    builtins.currentSystem == "aarch64-darwin"
    then (import sources.nix-gccemacs-darwin).pkgs.aarch64-darwin.emacsGccDarwin
    else pkgs.emacsNativeComp;
  nix-bisect = import sources.nix-bisect { inherit pkgs; };
  # nixpkgs-review = import sources.nixpkgs-review { inherit pkgs; };
  wrapWeb = pkgs.callPackage ./wrapWeb.nix { };
  wayland-packages = with pkgs; [
    ((emacsPackagesFor emacsPgtkGcc).emacsWithPackages (e: [ e.vterm ]))
    firefox-wayland
  ];
  web-shortcuts = with pkgs; [
    (wrapWeb "element" "https://app.element.io")
    (wrapWeb "hn" "https://news.ycombinator.com")
    (wrapWeb "neverssl" "http://neverssl.com")
    (wrapWeb "mastodon" "https://mastodon.social")
  ];
  linuxPackages = with pkgs; [
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
  ] ++ wayland-packages ++ web-shortcuts;
  darwinPackages = with pkgs; [
    coreutils
    gnused
    gccemacs
    spoof-mac
  ];
  # Package set to use when wanting to use x86-darwin packages on
  # aarch64-darwin.
  pkgs' = if isDarwin then x86-darwin-pkgs else pkgs;
  sharedPackages = with pkgs; [
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    bat
    bash
    borgbackup
    cabal-install
    cachix
    clang_13
    cmake
    # coqPackages_8_13.coquelicot
    # coqPackages_8_13.hierarchy-builder
    masterPkgs.coqPackages_8_13.QuickChick
    masterPkgs.coqPackages_8_13.simple-io
    masterPkgs.coqPackages_8_13.coq-ext-lib
    masterPkgs.coqPackages_8_13.mathcomp
    masterPkgs.coqPackages_8_13.coqhammer
    masterPkgs.coqPackages_8_13.coq-elpi
    masterPkgs.coqPackages_8_13.trakt
    masterPkgs.coqPackages_8_13.stdpp
    # masterPkgs.coqPackages_8_13.smtcoq
    # coqPackages_8_13.mathcomp-algebra
    # coqPackages_8_13.mathcomp-analysis
    # coqPackages_8_13.mathcomp-fingroup
    masterPkgs.coqPackages_8_13.mathcomp-ssreflect
    masterPkgs.coq_8_13
    curl
    ((import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/a3e1e9271e0ff87309d44f9817baadb09b305757.tar.gz";
    }) {}).cvc4)
    dejavu_fonts
    emscripten
    eprover
    ghostscript
    github-cli
    gnumake
    guile
    graphviz
    (import ./haskell-packages.nix { inherit pkgs; })
    haskellPackages.haskell-language-server
    hlint
    htop
    jq
    ledger
    mpv
    niv
    ocaml
    ocamlPackages.ocamlbuild
    nixpkgs-review
    nmap
    nodePackages.bash-language-server
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.pyright
    nodejs
    mosh
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
    tmux
    tor
    torsocks
    tree
    (tree-sitter.overrideAttrs (oA: { webUISupport = true; }))
    unzip
    vampire
    vim
    watch
    wget
    yt-dlp
    z3-tptp
    zathura
    zip
  ];
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)
