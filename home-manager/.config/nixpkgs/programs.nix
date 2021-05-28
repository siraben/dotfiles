{ lib, pkgs, isDarwin, isLinux }:

let
  darwinShellExtra = ''
    # For multi-user installation
    export NIX_PATH=$NIX_PATH:$HOME/.nix-defexpr/channels
    source $HOME/.nix/remote-build-env
  '';
  linuxShellExtra = "";
  sharedShellExtra = ''
      source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
  '';
in

{
  home-manager.enable = true;
  direnv.enable = true;
  direnv.enableNixDirenvIntegration = true;
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
    } // (lib.optionalAttrs isDarwin (import ./darwin-aliases.nix {}));
    initExtra = lib.concatStringsSep "\n"
      [
        (lib.optionalString isDarwin darwinShellExtra)
        (lib.optionalString isLinux linuxShellExtra)
        sharedShellExtra
      ];
  };
}
