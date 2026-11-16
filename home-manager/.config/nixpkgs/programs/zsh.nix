{
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
  linuxShellExtra = ''
    # Preserve development toolchains and NixOS's wrapper precedence.  Append
    # any missing profile paths, using the unique array to avoid duplicates.
    path=($path "$HOME/.nix-profile/bin" /nix/var/nix/profiles/default/bin
          "$HOME/.local/state/nix/profiles/home-manager/home-path/bin")
  '';
  sharedShellExtra = ''
    # The remote host/session identifies mosh tabs without its verbose prefix.
    export MOSH_TITLE_NOPREFIX=1

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
  programs.zsh = {
    enable = true;
    envExtra = lib.optionalString isDarwin ''
      # Non-interactive shells do not read /etc/zshrc's Nix setup.  The
      # installer script is guarded, so inherited development paths survive.
      if [[ -r /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]]; then
        . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
      fi
      typeset -U path
      path=($path "$HOME/.nix-profile/bin" /nix/var/nix/profiles/default/bin)
      if [[ -o login ]]; then
        # /etc/zprofile's path_helper will reorder PATH after .zshenv.
        typeset -ga siraben_pre_login_path=($path)
      fi
    '';
    profileExtra = lib.optionalString isDarwin ''
      # Restore the incoming order, retaining paths discovered by path_helper.
      # In particular, do not put global tools ahead of nix develop/direnv.
      path=("''${siraben_pre_login_path[@]}" $path)
      unset siraben_pre_login_path
    '';
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
    }
    // (lib.optionalAttrs isDarwin (import ../darwin-aliases.nix { }));
    initContent = lib.concatStringsSep "\n" [
      (lib.optionalString isLinux linuxShellExtra)
      sharedShellExtra
    ];
  };
}
