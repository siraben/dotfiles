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
    overlays = lib.optionals (!minimal) [
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
      tree-sitter-sml = { src = inputs.tree-sitter-sml; };
    };
  })).builtGrammars;
in
lib.recursiveUpdate (rec {
  nixpkgs = pkgsOptions;
  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/${home.username}" else "/home/${home.username}";
  home.packages = import ./packages.nix { inherit lib pkgs isDarwin isLinux minimal; };

  home.sessionVariables = {
    EDITOR = "emacsclient";
  } // (lib.optionalAttrs isDarwin {
    HOMEBREW_NO_AUTO_UPDATE = 1;
    HOMEBREW_NO_ANALYTICS = 1;
  });

  home.sessionPath = lib.optionals isDarwin [
    "/opt/homebrew/bin"
  ];

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
      new_tab
      cd ~
      launch zsh
    '';
    
    home.file.".config/kitty/tab_bar.py".text = ''
      import datetime
      from kitty.fast_data_types import Screen, add_timer, get_options
      from kitty.tab_bar import (
          DrawData,
          ExtraData,
          TabBarData,
          as_rgb,
          draw_tab_with_powerline,
      )

      timer_id = None

      def draw_tab(
          draw_data: DrawData,
          screen: Screen,
          tab: TabBarData,
          before: int,
          max_title_length: int,
          index: int,
          is_last: bool,
          extra_data: ExtraData,
      ) -> int:
          global timer_id
          if timer_id is None:
              timer_id = add_timer(_redraw_tab_bar, 1.0, True)
          
          # Draw the tab with powerline style
          end = draw_tab_with_powerline(
              draw_data, screen, tab, before, max_title_length, index, is_last, extra_data
          )
          
          # Draw clock on the right side if this is the last tab
          if is_last:
              date_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
              date_time_len = len(date_time) + 2  # Add padding

              # Always position the clock at the right edge
              clock_x = screen.columns - date_time_len

              # Only draw the clock if there's enough space
              if clock_x > end:
                  # Fill space between tab and clock
                  screen.cursor.x = end
                  screen.draw(" " * (clock_x - end))

                  # Draw the date/time
                  opts = get_options()
                  default_bg = as_rgb(int(draw_data.default_bg))
                  screen.cursor.fg = 0
                  screen.cursor.bg = default_bg
                  screen.cursor.x = clock_x
                  screen.draw(f" {date_time}")
          
          return screen.cursor.x

      def _redraw_tab_bar(timer_id):
          for tm in _get_tm():
              tm.mark_tab_bar_dirty()

      def _get_tm():
          from kitty.fast_data_types import get_boss
          boss = get_boss()
          for window in boss.all_windows:
              tabman = window.tabref()
              if tabman is not None:
                  yield tabman
    '';
})
