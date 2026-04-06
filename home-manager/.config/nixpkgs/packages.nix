{ lib, pkgs, isDarwin, isLinux, profile }:
let
  isMinimal = profile == "minimal";
  isFull = profile == "full";
  whenNotMinimal = lib.optionals (!isMinimal);
  whenFull = lib.optionals isFull;
  my-emacs = with pkgs; emacs.pkgs.withPackages (p: [ p.vterm ]);
  wayland-packages = whenFull (with pkgs; [
    firefox
  ]);
  linuxPackages = whenFull (with pkgs; [
    keepassxc
    kitty
    vlc
  ]) ++ wayland-packages;
  darwinPackages = with pkgs; [
    # GNU replacements
    coreutils
    gnused
    findutils
    gnugrep
    gawk
    diffutils
    gnutar
    patch

    rsync
    file
    less
    gzip
    bzip2

    pinentry_mac
  ];
  languageServers = with pkgs; [
    haskellPackages.haskell-language-server
    basedpyright
    rassumfrassum
    ruff
  ];
  sharedPackages = with pkgs; [
    bash
    curl
    htop
    vim
    watch
    wget
    mosh
    nixpkgs-review
    gh
    ranger
    croc
  ] ++ (whenNotMinimal ([
    # CLI tools (headless + full)
    claude-code
    codex
    mosh2
    bat
    borgbackup
    cachix
    gnumake
    jq
    killall
    nix-output-monitor
    ripgrep
    shellcheck
    stow
    tldr
    tree
    zip
  ])) ++ (whenFull ([
    # Development tools (full only)
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    cabal-install
    cargo
    dejavu_fonts
    ffmpeg
    (pkgs.nerd-fonts.jetbrains-mono)
    github-cli
    (import ./haskell-packages.nix { inherit pkgs; })
    hlint
    imagemagick
    ledger
    my-emacs
    niv
    nodejs
    (import ./python-packages.nix { inherit pkgs; })
    rust-analyzer
    # (import ./texlive-packages.nix { inherit pkgs; })
    tree-sitter
    typst
    # uncommenting until deno gets fixed
    # mpv
    # yt-dlp
  ] ++ languageServers));
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)
