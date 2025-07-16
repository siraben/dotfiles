{ config, lib, currentSystem, minimal, inputs, ... }:

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
      (import ./overlay.nix)
    ];
    config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) unfreePackages;
  };
  pkgs = import inputs.nixpkgs {
    system = currentSystem;
    inherit (pkgsOptions) overlays config;
  };
  grammars = (pkgs.tree-sitter.override (with pkgs; {
    extraGrammars = {
      tree-sitter-promela = { src = inputs.tree-sitter-promela; };
      tree-sitter-formula = { src = inputs.tree-sitter-formula; };
      tree-sitter-sml = { src = inputs.tree-sitter-sml; };
      tree-sitter-cool = { src = inputs.tree-sitter-cool; };
    };
  })).builtGrammars;
in
lib.recursiveUpdate (rec {
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/${home.username}" else "/home/${home.username}";
  home.packages = import ./packages.nix { inherit lib pkgs isDarwin isLinux minimal; };

  home.sessionVariables = {
    EDITOR = "emacsclient";
  } // (lib.optionalAttrs isLinux {
    XDG_CURRENT_DESKTOP = "sway";
    MOZ_ENABLE_WAYLAND = 1;
  }) // (lib.optionalAttrs isDarwin {
    HOMEBREW_NO_AUTO_UPDATE = 1;
    HOMEBREW_NO_ANALYTICS = 1;
  });

  home.language = {
    ctype = "en_US.UTF-8";
    base = "en_US.UTF-8";
  };

  programs = import ./programs.nix { inherit lib pkgs isDarwin isLinux; };
  fonts.fontconfig.enable = true;
  services = lib.optionalAttrs isLinux (import ./services.nix { inherit lib pkgs; });
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false;
})
(lib.optionalAttrs (!minimal) {
    home.file.".tree-sitter".source = (pkgs.runCommand "grammars" {} ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: ''
          name=${name}
          ln -s ${src}/parser $out/bin/''${name#tree-sitter-}.so
        '')
        grammars)}
  '');
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
      new_tab Home
      cd ~
      launch zsh
    '';
})
