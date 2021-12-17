{ lib, pkgs, isDarwin, isLinux }:

let
  darwinShellExtra = ''
    if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix.sh' ]; then . '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'; fi

    if [ -e "$HOME/.nix/remote-build-env" ]; then . "$HOME/.nix/remote-build-env"; fi
  '';
  linuxShellExtra = ''
    export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
  '';
  sharedShellExtra = ''
    fpath+=("${pkgs.pure-prompt}/share/zsh/site-functions")
    if [ "$TERM" != dumb ]; then
      autoload -U promptinit && promptinit && prompt pure
    fi
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
  '';
in

{
  command-not-found.enable = true;
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
      hm = "home-manager";
      hms = "home-manager switch";
      httpcode = ''curl -o /dev/null -s -w "%{http_code}\n"'';
      nb = "nix build";
      nc = "nix channel";
      ncg = "nix-collect-garbage";
      nd = "nix develop";
      ne = "nix edit";
      nr = "nix repl";
      nrep = "nix-review pr --post-result";
      nrp = "nix repl '<nixpkgs>'";
      ns = "nix-shell";
      tb = "tput bel";
      ix = ''curl -n -F 'f:1=<-' http://ix.io'';
    } // (lib.optionalAttrs isDarwin (import ./darwin-aliases.nix {}));
    initExtra = lib.concatStringsSep "\n"
      [
        (lib.optionalString isDarwin darwinShellExtra)
        (lib.optionalString isLinux linuxShellExtra)
        sharedShellExtra
      ];
  };
}
