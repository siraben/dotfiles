{ config, lib, currentSystem, profile, inputs, ... }:

let
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  unfreePackages = [
    "discord"
    "slack"
    "spotify"
    "spotify-unwrapped"
    "zoom"
    "aspell-dict-en-science"
    "claude-code"
  ];
  pkgsOptions = {
    overlays = [
      (import ./overlay.nix { inherit inputs; })
      inputs.siraben-overlay.overlays.default
    ];
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
  pkgs = import inputs.nixpkgs {
    system = currentSystem;
    inherit (pkgsOptions) overlays config;
  };
in
lib.recursiveUpdate (rec {
  nixpkgs = pkgsOptions;
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/${home.username}" else "/home/${home.username}";
  home.packages = import ./packages.nix { inherit lib pkgs isDarwin isLinux profile; };

  home.sessionVariables = {
    EDITOR = "vim";
    TZ = "America/Los_Angeles";
  } // (lib.optionalAttrs isLinux {
    BUN_INSTALL = "/home/siraben/cold/toolchains/bun";
    CARGO_HOME = "/home/siraben/cold/toolchains/cargo";
    RUSTUP_HOME = "/home/siraben/cold/toolchains/rustup";
    npm_config_cache = "/home/siraben/cold/caches/npm";
    npm_config_prefix = "/home/siraben/cold/toolchains/npm-global";
  }) // (lib.optionalAttrs isDarwin {
    HOMEBREW_NO_AUTO_UPDATE = 1;
    HOMEBREW_NO_ANALYTICS = 1;
  });

  home.sessionPath = (lib.optionals isLinux [
    "/home/siraben/cold/toolchains/bun/bin"
    "/home/siraben/cold/toolchains/npm-global/bin"
    "/home/siraben/cold/toolchains/cargo/bin"
  ]) ++ (lib.optionals isDarwin [
    "/opt/homebrew/bin"
  ]);

  home.file = lib.optionalAttrs isLinux {
    ".bunfig.toml".text = ''
      [install]
      cache = "/home/siraben/cold/caches/bun"
    '';
    ".npmrc".text = ''
      cache=/home/siraben/cold/caches/npm
      prefix=/home/siraben/cold/toolchains/npm-global
    '';
    ".config/baloofilerc".text = ''
      [Basic Settings]
      Indexing-Enabled=false
    '';
  } // lib.optionalAttrs (profile == "headless") {
    ".agent-deck/config.toml".source =
      (pkgs.formats.toml { }).generate "agent-deck-config" {
        default_tool = "claude";
        theme = "dark";
        worktree.branch_prefix = "siraben/";
        worktree.default_location = "sibling";
        claude.dangerous_mode = true;
        feedback.disabled = true;
      };
  };

  home.language = {
    ctype = "en_US.UTF-8";
    base = "en_US.UTF-8";
  };

  programs = import ./programs.nix { inherit lib pkgs isDarwin isLinux profile; };
  fonts.fontconfig.enable = true;
  services = lib.optionalAttrs isLinux (import ./services.nix { inherit lib pkgs; });
  nix.package = lib.mkDefault pkgs.nix;
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
  };
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;
})
(lib.optionalAttrs (profile == "full") {
    home.file.".config/kitty/session.conf".text = ''
      # Kitty session file
      # This will restore your tabs and windows when kitty starts

      # You can have multiple layouts, each in its own tab
      # Each layout can have multiple windows

      # Example session with multiple tabs
      # Uncomment and modify as needed:

      # Tab 1 - Development
      # new_tab Development
      # cd ~/projects
      # launch zsh

      # Tab 2 - System monitoring
      # new_tab Monitoring
      # layout tall
      # launch htop
      # launch --location=vsplit watch -n 1 "df -h"

      # Tab 3 - Logs
      # new_tab Logs
      # cd /var/log
      # launch tail -f system.log

      # Simple session that just opens a single tab in home directory
      new_tab
      cd ~
      launch zsh
    '';

    home.file.".config/kitty/tab_bar.py".source = ./tab_bar.py;
})
