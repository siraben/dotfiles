{ lib, pkgs, isDarwin, isLinux, profile }:
let
  isMinimal = profile == "minimal";
  isFull = profile == "full";
  isHeadless = profile == "headless";
  whenNotMinimal = lib.optionals (!isMinimal);
  whenFull = lib.optionals isFull;
  whenHeadless = lib.optionals isHeadless;
  # Codex's automatic backend detection loses sight of Kitty after SSH, Mosh,
  # or tmux changes the terminal environment. Force OSC 9; Codex wraps it in
  # tmux's DCS passthrough sequence whenever it detects a tmux session.
  codex-with-terminal-notifications = pkgs.writeShellApplication {
    name = "codex";
    text = ''
      codex_args=(
        --config 'tui.notifications=true'
        --config 'tui.notification_method="osc9"'
      )

      # SSH forwards TERM but not TMUX. When SSH itself is running inside
      # tmux, give Codex a process-local marker so it wraps OSC 9 for the
      # upstream tmux. Keep that synthetic marker out of agent subprocesses.
      if [[ -z "''${TMUX:-}" && "''${TERM:-}" == tmux* ]]; then
        export TMUX=/dev/null,0,0
        codex_args+=(
          --config 'shell_environment_policy.filters={ TMUX = "exclude" }'
        )
      fi

      exec ${pkgs.codex}/bin/codex \
        "''${codex_args[@]}" \
        "$@"
    '';
  };
  my-emacs = with pkgs; emacs.pkgs.withPackages (p: [ p.vterm ]);
  wayland-packages = whenFull (with pkgs; [
    firefox
  ]);
  linuxPackages = whenFull (with pkgs; [
    keepassxc
    kitty
    vlc
  ]) ++ wayland-packages ++ (with pkgs; [
    forgejo-mcp
  ]);
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
    tea
    ranger
    croc
  ] ++ (whenNotMinimal ([
    # CLI tools (headless + full)
    agent-deck
    claude-code
    codex-with-terminal-notifications
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
