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
    EDITOR = "emacsclient";
    TZ = "America/Los_Angeles";
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

  programs = import ./programs.nix { inherit lib pkgs isDarwin isLinux profile; };
  fonts.fontconfig.enable = true;
  services = lib.optionalAttrs isLinux (import ./services.nix { inherit lib pkgs; });
  nix.package = lib.mkDefault pkgs.nix;
  nix.settings = {
    experimental-features = [ "nix-command" "flakes" ];
    keep-derivations = true;
    keep-outputs = true;
    builders-use-substitutes = true;
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://siraben.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "siraben.cachix.org-1:/zSVUB18DWcjQF52VMh0v7MzjI+pdevnWOa01koPoYc="
    ];
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
