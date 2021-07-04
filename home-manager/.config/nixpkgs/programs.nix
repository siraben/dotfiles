{ lib, pkgs, isDarwin, isLinux }:

let
  darwinShellExtra = ''
    # For multi-user installation
    export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH

    if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix.sh' ]; then . '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'; fi

    source $HOME/.nix/remote-build-env
  '';
  linuxShellExtra = "";
  sharedShellExtra = ''
  '';
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
    userName = "Ben Siraphob";
    userEmail = "bensiraphob@gmail.com";
    extraConfig = {
      pull.rebase = true;
      github.user = "siraben";
    };
  };
  # emacs = {
  #   enable = true;
  #   package = if isDarwin then pkgs.emacsGcc else pkgs.emacsPgtkGcc;
  # };
  zsh = {
    enable = true;
    oh-my-zsh = {
      enable = true;
      theme = "robbyrussell";
      plugins = [ "git" "autojump" ];
    };
    history = {
      size = 100000;
      save = 100000;
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
      ssh = "kitty +kitten ssh";
    } // (lib.optionalAttrs isDarwin (import ./darwin-aliases.nix {}));
    initExtra = lib.concatStringsSep "\n"
      [
        (lib.optionalString isDarwin darwinShellExtra)
        (lib.optionalString isLinux linuxShellExtra)
        sharedShellExtra
      ];
  };
}
