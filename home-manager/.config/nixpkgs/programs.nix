{ lib, pkgs, isDarwin, isLinux }:

let
  linuxShellExtra = ''
    export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
  '';
  sharedShellExtra = ''
    fpath+=("${pkgs.pure-prompt}/share/zsh/site-functions")
    if [ "$TERM" != dumb ]; then
      autoload -U promptinit && promptinit && prompt pure
      vterm_printf(){
          if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }
      vterm_prompt_end() {
          vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
      }
      setopt PROMPT_SUBST
      PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
    else
      unsetopt zle
      PS1='$ '
    fi
  '';
in

{
  direnv = {
    enable = true;
    stdlib = ''
      : ''${XDG_CACHE_HOME:=$HOME/.cache}
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
          echo "''${direnv_layout_dirs[$PWD]:=$(
              echo -n "$XDG_CACHE_HOME"/direnv/layouts/
              echo -n "$PWD" | shasum | cut -d ' ' -f 1
          )}"
      }
    '';
    nix-direnv.enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
  };
  gpg.enable = true;
  git = {
    enable = true;
    lfs.enable = true;
    signing = {
      key = "45F0E5D788143267";
      signByDefault = true;
    };
    settings = {
      user.name = "Ben Siraphob";
      user.email = "bensiraphob@gmail.com";
      pull.rebase = true;
      github.user = "siraben";
      advice.detachedHead = false;
      core.commitGraph = true;
      fetch.writeCommitGraph = true;
      # Performance improvements
      core.preloadIndex = true;
      core.fscache = true;
      core.untrackedCache = true;
      feature.manyFiles = true;
      gc.writeCommitGraph = true;
      # Diff performance
      diff.algorithm = "histogram";
    };
  };
  autojump.enable = true;
  mcfly.enable = true;
  tmux = {
    enable = true;
    clock24 = true;
    baseIndex = 1;
    historyLimit = 200000;
    mouse = true;
    terminal = "tmux-256color";
    # tmux defaults and key bindings shared across hosts
    extraConfig = ''
      # Truecolor / 24-bit color
      set -as terminal-overrides ",xterm-256color:Tc,xterm-kitty:Tc,tmux*:Tc"
      # Open new windows/panes in the current working directory
      bind c new-window -c "#{pane_current_path}"

      # Session defaults
      set -g pane-border-style 'fg=#5a5a5a'
      set -g pane-active-border-style 'fg=#00afff'
      set -g allow-rename off
    '';
  };
  kitty = {
    enable = true;
    settings = {
      font_family = "JetBrainsMono Nerd Font";
      cursor_blink_interval = 0;
      scrollback_lines = 10000;
      scrollback_pager = "less --chop-long-lines --RAW-CONTROL-CHARS +INPUT_LINE_NUMBER";
      scrollback_pager_history_size = 1000;
      startup_session = "session.conf";
      macos_option_as_alt = "yes";
      # Mouse settings
      mouse_hide_wait = 0;
      # Tab bar configuration
      tab_bar_edge = "top";
      tab_bar_style = "custom";
      tab_powerline_style = "slanted";
      tab_title_template = "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{title}";
      active_tab_title_template = "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{title} [{num_windows} window{'s' if num_windows > 1 else ''}]";
      tab_bar_background = "#333";
      active_tab_foreground = "#fff";
      active_tab_background = "#666";
      inactive_tab_foreground = "#ccc";
      inactive_tab_background = "#333";
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
  zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = lib.mkForce "";
      extraConfig = ''
        ZSH_THEME=""
      '';
      plugins = [ "git" ];
    };
    history = {
      size = 100000;
      save = 100000;
      extended = true;
    };
    shellAliases = {
      nb = "nix build";
      nbi = "nix build --impure";
      ncg = "nix-collect-garbage";
      nd = "nix develop";
      ne = "nix edit";
      nr = "nix repl";
      nreps = "nix-review pr --post-result";
      nrep = "nix-review pr --post-result --no-shell";
    } // (lib.optionalAttrs isDarwin (import ./darwin-aliases.nix {}));
    envExtra = lib.optionalString isDarwin ''
      # Source Nix daemon for all shells (including SSH)
      if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
        . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      fi
    '';
    initContent = lib.concatStringsSep "\n"
      [
        (lib.optionalString isLinux linuxShellExtra)
        sharedShellExtra
      ];
  };
}
