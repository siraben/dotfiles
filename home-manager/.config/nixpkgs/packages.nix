{ lib, sources, pkgs, isDarwin, isLinux }:
let
  gccemacs = (import sources.nix-gccemacs-darwin).emacsGccDarwin;
  nix-bisect = import sources.nix-bisect { inherit pkgs; };
  nixpkgs-review = import sources.nixpkgs-review { inherit pkgs; };
  nix-eval-lsp = import sources.nix-eval-lsp;
  wrapWeb = pkgs.callPackage ./wrapWeb.nix { };
  wayland-packages = with pkgs; [
    emacsPgtkGcc
    firefox-wayland
    grim
    slurp
    swaylock-fancy
    wlsunset
    wofi
    xdg-desktop-portal-wlr
  ];
  web-shortcuts = with pkgs; [
    (wrapWeb "element" "https://app.element.io")
    (wrapWeb "hn" "https://news.ycombinator.com")
    (wrapWeb "neverssl" "http://neverssl.com")
    (wrapWeb "mastodon" "https://mastodon.social")
  ];
  linuxPackages = with pkgs; [
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
    nix-update
    offlineimap
    paper-icon-theme
    poppler_utils
    rhythmbox
    rofi
    slack
    spotify
    system-config-printer
    tdesktop
    thunderbird
    tor-browser-bundle-bin
    transmission-gtk
    ungoogled-chromium
    vlc
    whois
    wpa_supplicant
    xorg.xkill
    zoom-us
  ] ++ wayland-packages ++ web-shortcuts;
  darwinPackages = with pkgs; [
    cocoapods
    coreutils
    emacsGcc
  ];
  sharedPackages = with pkgs; [
    ag
    alacritty
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    autojump
    bat
    borgbackup
    cabal-install
    cachix
    ccls
    chez
    clang-tools
    clang_10
    cmake
    coq_8_12
    coqPackages.coquelicot
    exiftool
    (haskellPackages.ghcWithHoogle (h: [ h.arithmoi h.QuickCheck h.vector ]))
    github-cli
    gnumake
    graphviz
    guile
    haskellPackages.ghcide
    haskellPackages.haskell-language-server
    hlint
    htop
    jq
    kitty
    ledger
    manix
    mpv
    mu
    niv
    nix-bisect
    nix-eval-lsp
    nix-index
    nix-prefetch-git
    nixfmt
    nixpkgs-fmt
    nixpkgs-review
    nodePackages.bash-language-server
    nodePackages.javascript-typescript-langserver
    nodePackages.pyright
    nodejs
    python38
    python38Packages.nix-prefetch-github
    ranger
    ripgrep
    rmview
    rustup
    shellcheck
    stow
    (texlive.combine {
      inherit (texlive) scheme-small latexmk wrapfig rotfloat capt-of minted fvextra upquote catchfile xstring framed biblatex csquotes;
    })
    the-powder-toy
    tldr
    tmux
    tnef
    tor
    tree
    unzip
    vim
    watch
    youtube-dl
    zathura
    zip
  ];
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)