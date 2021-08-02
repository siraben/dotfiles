{ lib, pkgs, isDarwin, isLinux }:

let
  darwinShellExtra = ''
    # For multi-user installation
    export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH

    if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix.sh' ]; then . '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'; fi

    source $HOME/.nix/remote-build-env
  '';
  linuxShellExtra = "";
  sharedShellExtra = "";
in

{
  home-manager.enable = true;
  nix-index.enable = true;
  direnv = {
    enable = true;
    nix-direnv.enable = true;
    nix-direnv.enableFlakes = true;
    enableZshIntegration = true;
  };
  broot.enable = true;
  git = {
    enable = true;
    lfs.enable = true;
    userName = "Ben Siraphob";
    userEmail = "bensiraphob@gmail.com";
    extraConfig = {
      pull.rebase = true;
      github.user = "siraben";
    };
  };
  autojump.enable = true;
  zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
    };
    history = {
      size = 100000;
      save = 100000;
      extended = true;
    };
    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
      {
        name = "powerlevel10k-config";
        src = pkgs.lib.cleanSource ./p10k;
        file = "p10k-pure.zsh";
      }
    ];
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
