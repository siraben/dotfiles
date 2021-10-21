{ lib, sources, pkgs, pkgsStable, isDarwin, isLinux }:
let
  gccemacs = (import sources.nix-gccemacs-darwin).pkgs.x86_64-darwin.emacsGccDarwin;
  nix-bisect = import sources.nix-bisect { inherit pkgs; };
  nixpkgs-review = import sources.nixpkgs-review { inherit pkgs; };
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
    alacritty
    anki
    bemenu
    discord
    docker
    docker-compose
    dragon-drop
    evince
    exfat
    feh
    gnome3.cheese
    gnupg
    keepassxc
    killall
    kitty
    libreoffice
    lightlocker
    msmtp
    musescore
    networkmanager
    nextcloud-client
    nix-update
    offlineimap
    paper-icon-theme
    poppler_utils
    racket
    rhythmbox
    slack
    spotify
    system-config-printer
    tdesktop
    thunderbird
    # tor-browser-bundle-bin
    transmission-gtk
    ungoogled-chromium
    vlc
    whois
    wpa_supplicant
    xorg.xkill
    zoom-us
  ] ++ wayland-packages ++ web-shortcuts;
  darwinPackages = with pkgs; [
    coreutils
    gccemacs
    spoof-mac
  ];
  sharedPackages = with pkgs; [
    ag
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    bat
    bash
    borgbackup
    cabal-install
    cachix
    ccls
    chez
    clang-tools
    clang_12
    cmake
    coqPackages_8_13.coquelicot
    coqPackages_8_13.hierarchy-builder
    coqPackages_8_13.mathcomp
    coqPackages_8_13.mathcomp-algebra
    coqPackages_8_13.mathcomp-analysis
    coqPackages_8_13.mathcomp-fingroup
    coqPackages_8_13.mathcomp-ssreflect
    coq_8_13
    dejavu_fonts
    exiftool
    github-cli
    gnumake
    graphviz
    guile
    (haskellPackages.ghcWithHoogle (h: [ h.arithmoi h.QuickCheck h.vector ]))
    haskellPackages.ghcide
    haskellPackages.haskell-language-server
    hlint
    htop
    hyperfine
    jq
    ledger
    mpv
    niv
    nix-prefetch-git
    nixfmt
    nixpkgs-fmt
    nixpkgs-review
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodePackages.pyright
    nodejs
    python38
    python38Packages.nix-prefetch-github
    ranger
    ripgrep
    rmview
    rnix-lsp
    rustup
    shellcheck
    stow
    swiProlog
    pkgsStable.texlab
    (texlive.combine {
      inherit (texlive) amsmath scheme-small latexmk wrapfig rotfloat capt-of minted fvextra upquote catchfile xstring framed biblatex csquotes;
    })
    the-powder-toy
    tldr
    tmux
    tnef
    tor
    tree
    unzip
    vim
    w3m
    watch
    wget
    youtube-dl
    zathura
    zip
  ];
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)
