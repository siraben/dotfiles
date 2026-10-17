{
  lib,
  pkgs,
  isDarwin,
  isLinux,
}:

let
  linuxShellExtra = ''
    # Every interactive shell prepends the active Home Manager and Nix profiles
    # so tmux sessions use the selected profile.
    export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$HOME/.local/state/nix/profiles/home-manager/home-path/bin:$PATH"
    export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
    export SSH_AUTH_SOCK="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/ssh-agent"
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
    }
    // (lib.optionalAttrs isDarwin (import ../darwin-aliases.nix { }));
    initContent = lib.concatStringsSep "\n" [
      # path_helper runs before this block, so prepend the Nix paths directly.
      (lib.optionalString isDarwin ''
        export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"
      '')
      (lib.optionalString isLinux linuxShellExtra)
      sharedShellExtra
    ];
  };
}
