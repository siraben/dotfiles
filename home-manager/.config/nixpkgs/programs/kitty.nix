{
  pkgs,
  isDarwin,
  profile,
}:

{
  kitty = {
    enable = profile == "full";
    # On macOS, install the app with Homebrew while retaining this managed config.
    package = if isDarwin then null else pkgs.kitty;
    settings = {
      font_family = "JetBrainsMono Nerd Font";
      cursor_blink_interval = 0;
      scrollback_lines = 10000;
      scrollback_pager = "less --chop-long-lines --RAW-CONTROL-CHARS +INPUT_LINE_NUMBER";
      scrollback_pager_history_size = 100;
      startup_session = "~/.config/kitty/sessions/last-session.conf";
      macos_option_as_alt = "yes";
      # Mouse settings
      mouse_hide_wait = 0;
      # Window appearance
      placement_strategy = "center";
      inactive_text_alpha = -0.9;
      macos_titlebar_color = "background";
      # Tab bar configuration
      tab_bar_edge = "top";
      tab_bar_style = "custom";
      tab_powerline_style = "angled";
      tab_activity_symbol = "● ";
      tab_title_template = "{fmt.fg.color1}{bell_symbol}{secure_input_symbol}{fmt.fg.color2}{activity_symbol}{fmt.fg.tab}{sup.index} {tab.last_focused_progress_percent}{title}{' [' + str(num_windows) + 'w]' if num_windows > 1 else ''}";
      tab_bar_background = "#000000";
      active_tab_foreground = "#000000";
      active_tab_background = "#7aa6da";
      active_tab_font_style = "bold";
      inactive_tab_foreground = "#969896";
      inactive_tab_background = "#222222";
      inactive_tab_font_style = "normal";
      # Disable macOS menu bar title updates
      macos_show_window_title_in = "window";

      # Tomorrow Night Bright theme colors
      foreground = "#eaeaea";
      background = "#000000";
      selection_foreground = "#000000";
      selection_background = "#424242";
      cursor = "#eaeaea";
      cursor_text_color = "#000000";
      url_color = "#70c0b1";
      active_border_color = "#969896";
      inactive_border_color = "#2a2a2a";
      bell_border_color = "#d54e53";

      # Black
      color0 = "#000000";
      color8 = "#969896";

      # Red
      color1 = "#d54e53";
      color9 = "#d54e53";

      # Green
      color2 = "#b9ca4a";
      color10 = "#b9ca4a";

      # Yellow
      color3 = "#e7c547";
      color11 = "#e7c547";

      # Blue
      color4 = "#7aa6da";
      color12 = "#7aa6da";

      # Magenta
      color5 = "#c397d8";
      color13 = "#c397d8";

      # Cyan
      color6 = "#70c0b1";
      color14 = "#70c0b1";

      # White
      color7 = "#eaeaea";
      color15 = "#ffffff";
    };
    shellIntegration.enableZshIntegration = true;
    keybindings = {
      "kitty_mod+t" = "new_tab_with_cwd";
      "cmd+t" = "new_tab_with_cwd";
      "kitty_mod+enter" = "launch --cwd=current";
      "cmd+enter" = "launch --cwd=current";
      "cmd+s" =
        "save_as_session --save-only --use-foreground-process ~/.config/kitty/sessions/last-session.conf";
      "cmd+q" =
        "combine : save_as_session --save-only --use-foreground-process ~/.config/kitty/sessions/last-session.conf : quit";
      # Switch tabs with cmd+number
      "cmd+1" = "goto_tab 1";
      "cmd+2" = "goto_tab 2";
      "cmd+3" = "goto_tab 3";
      "cmd+4" = "goto_tab 4";
      "cmd+5" = "goto_tab 5";
      "cmd+6" = "goto_tab 6";
      "cmd+7" = "goto_tab 7";
      "cmd+8" = "goto_tab 8";
      "cmd+9" = "goto_tab 9";
    };
  };
}
