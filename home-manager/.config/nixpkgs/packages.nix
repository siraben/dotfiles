{ lib, pkgs, isDarwin, isLinux, minimal }:
let
  whenNotMinimal = lib.optionals (!minimal);
  gccemacs = with pkgs; ((emacsPackagesFor (emacs30.override { withNativeCompilation = false; })).emacsWithPackages (e: [ 
 e.vterm
 ]));
  wrapWeb = pkgs.callPackage ./wrapWeb.nix { };
  wayland-packages = whenNotMinimal (with pkgs; [
    gccemacs
    firefox-wayland
  ]);
  linuxPackages = whenNotMinimal (with pkgs; [
    keepassxc
    killall
    kitty
    nix-update
    vlc
    whois
  ]) ++ wayland-packages;
  darwinPackages = with pkgs; [
    coreutils
    gnused
    gccemacs
    spoof-mac
    pinentry_mac
  ];
  coqPackages = with pkgs; [
    # coqPackages_8_17.hierarchy-builder
    # coqPackages_8_17.QuickChick
    # coqPackages_8_17.mathcomp
    coqPackages_8_17.coq-hammer
    coqPackages_8_17.coq-hammer-tactics
    # coqPackages_8_17.mathcomp-ssreflect
    # coqPackages_8_17.ITree
    # coqPackages_8_17.coq-elpi
    coq_8_17
    # External provers for coq-hammer
    eprover
    vampire
    z3-tptp
  ];
  languageServers = with pkgs; [
    haskellPackages.haskell-language-server
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    pyright
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
    ollama
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
