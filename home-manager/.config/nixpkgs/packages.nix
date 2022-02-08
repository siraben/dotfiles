{ lib, sources, pkgs, pkgsStable, x86-darwin-pkgs, isDarwin, isLinux }:
let
  gccemacs = (import sources.nix-gccemacs-darwin).pkgs.x86_64-darwin.emacsGccDarwin;
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
  sharedPackages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    bat
    bash
    borgbackup
    cabal-install
    cachix
    clang_12
    cmake
    # (x86-darwin-pkgs.coqPackages_8_13).coquelicot
    # (x86-darwin-pkgs.coqPackages_8_13).hierarchy-builder
    (x86-darwin-pkgs.coqPackages_8_13).QuickChick
    (x86-darwin-pkgs.coqPackages_8_13).simple-io
    (x86-darwin-pkgs.coqPackages_8_13).coq-ext-lib
    (x86-darwin-pkgs.coqPackages_8_13).mathcomp
    # (x86-darwin-pkgs.coqPackages_8_13).mathcomp-algebra
    # (x86-darwin-pkgs.coqPackages_8_13).mathcomp-analysis
    # (x86-darwin-pkgs.coqPackages_8_13).mathcomp-fingroup
    (x86-darwin-pkgs.coqPackages_8_13).mathcomp-ssreflect
    x86-darwin-pkgs.coq_8_13
    dejavu_fonts
    ghostscript
    github-cli
    gnumake
    guile
    (haskellPackages.ghcWithHoogle (h: [ h.QuickCheck h.vector ]))
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
    nodePackages.bash-language-server
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.pyright
    nodejs
    python38
    python3Packages.pylint
    ranger
    ripgrep
    x86-darwin-pkgs.rmview
    rustup
    shellcheck
    stow
    swiProlog
    (texlive.combine {
      inherit (texlive) amsmath scheme-small latexmk wrapfig rotfloat capt-of minted fvextra upquote catchfile xstring framed biblatex csquotes preprint;
    })
    x86-darwin-pkgs.the-powder-toy
    tldr
    tmux
    tor
    tree
    unzip
    vim
    watch
    wget
    yt-dlp
    zathura
    zip
  ];
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)
