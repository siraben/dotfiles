{ lib, pkgs, isDarwin, isLinux }:

let
  darwinShellExtra = ''
    if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix.sh' ]; then . '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'; fi
    if [ -d "/opt/homebrew/bin" ]; then
      export PATH=/opt/homebrew/bin:$PATH
    fi
  '';
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
    userName = "Ben Siraphob";
    userEmail = "bensiraphob@gmail.com";
    signing = {
      key = "45F0E5D788143267";
      signByDefault = true;
    };
    extraConfig = {
      pull.rebase = true;
      github.user = "siraben";
      advice.detachedHead = false;
    };
  };
  autojump.enable = true;
  mcfly.enable = true;
  tmux = {
    enable = true;
    clock24 = true;
    baseIndex = 1;
    # open new windows in the same cwd
    extraConfig = ''
      bind c new-window -c "#{pane_current_path}"
    '';
  };
  kitty = {
    enable = true;
    font = {
      name = "DejaVu Sans Mono";
    };
    settings = {
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
      tab_bar_style = "powerline";
      tab_powerline_style = "slanted";
      tab_title_template = "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{title}";
      active_tab_title_template = "{fmt.fg.red}{bell_symbol}{activity_symbol}{fmt.fg.tab}{title} [{num_windows} window{'s' if num_windows > 1 else ''}]";
      tab_bar_background = "#333";
      active_tab_foreground = "#fff";
      active_tab_background = "#666";
      inactive_tab_foreground = "#ccc";
      inactive_tab_background = "#333";
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
    initContent = lib.concatStringsSep "\n"
      [
        (lib.optionalString isDarwin darwinShellExtra)
        (lib.optionalString isLinux linuxShellExtra)
        sharedShellExtra
      ];
  };
}
