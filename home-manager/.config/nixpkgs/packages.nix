{ lib, pkgs, isDarwin, isLinux, minimal }:
let
  whenNotMinimal = lib.optionals (!minimal);
  my-emacs = with pkgs; emacs.pkgs.withPackages (p: [ p.vterm ]);
  wayland-packages = whenNotMinimal (with pkgs; [
    firefox-wayland
  ]);
  linuxPackages = whenNotMinimal (with pkgs; [
    keepassxc
    kitty
    vlc
  ]) ++ wayland-packages;
  darwinPackages = with pkgs; [
    coreutils
    gnused
    pinentry_mac
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
    nixpkgs-review
  ] ++ (whenNotMinimal ([
    (aspellWithDicts (d: with d; [ en en-computers en-science ]))
    bat
    borgbackup
    cabal-install
    cachix
    cargo
    claude-code
    cmake
    dejavu_fonts
    ffmpeg
    github-cli
    gnumake
    (import ./haskell-packages.nix { inherit pkgs; })
    hlint
    imagemagick
    jq
    killall
    ledger
    mpv
    my-emacs
    niv
    nodejs
    ollama
    (import ./python-packages.nix { inherit pkgs; })
    ranger
    ripgrep
    rust-analyzer
    shellcheck
    stack
    stow
    # (import ./texlive-packages.nix { inherit pkgs; })
    tldr
    tree
    tree-sitter
    typst
    yt-dlp
    zip
  ] ++ languageServers));
in
sharedPackages ++ (lib.optionals isLinux linuxPackages) ++ (lib.optionals isDarwin darwinPackages)
