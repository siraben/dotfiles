{ config, lib, currentSystem, ... }:
# To put pkgs in scope in nix repl
# :a (import (import nix/sources.nix).nixpkgs) {}
let
  inherit (builtins) currentSystem;
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  # Fork of comma that uses local nix-index if possible.
  comma = (import
    (pkgs.fetchFromGitHub {
      owner = "SuperSandro2000";
      repo = "comma";
      rev = "28f94a11114893506ea2c5c5bdbc1dae1d1d8159";
      sha256 = "05157wn3f1whq6krcijhipkjw5zjml5h4xbf06ibd77kc57lqw8z";
    })
    { });
  cargo2nix = (import
    (pkgs.fetchFromGitHub {
      owner = "tenx-tech";
      repo = "cargo2nix";
      rev = "a0f38b977596c39f4127b67c84a6930b3cbd662a";
      sha256 = "0dq2nc7n4clvxm1592dr1s8d4gqy0pq6z1xlxy1dfmf18hij4k6d";
    })
    { }).package;
  linuxPackages = with pkgs; [
    anki
    arc-theme
    brave
    discord
    dmenu
    docker
    docker-compose
    evince
    exfat
    feh
    firefox
    gnome3.cheese
    gnumake
    gnupg
    keepassxc
    killall
    libreoffice
    lightlocker
    msmtp
    musescore
    networkmanager
    nextcloud-client
    nitrogen
    offlineimap
    paper-icon-theme
    poppler_utils
    rhythmbox
    rofi
    scrot
    slack
    spotify
    system-config-printer
    tdesktop
    thunderbird
    tor-browser-bundle-bin
    transmission-gtk
    vlc
    whois
    wpa_supplicant
    xorg.xkill
    zoom-us
  ];
  gccemacs = (import (pkgs.fetchFromGitHub {
    owner = "twlz0ne";
    repo = "nix-gccemacs-darwin";
    rev = "c10323fd2253d7b73bd6e06dd2cead5b3231df52";
    sha256 = "0bpm5isv687lf13lhi9ad6zaj820g5144293c114gxxlhl32f1wh";
  })).emacsGccDarwin;
  darwinPackages = with pkgs; [
    coreutils
    gccemacs
  ];
  sharedPackages = with pkgs; [
    ag
    agda
    alacritty
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    autojump
    bat
    borgbackup
    cabal-install
    cachix
    cargo2nix
    ccls
    chez
    clang-tools
    clang_11
    cmake
    comma
    coq
    exiftool
    ghc
    github-cli
    guile
    haskellPackages.ghcide
    haskellPackages.haskell-language-server
    hlint
    htop
    jq
    kitty
    ledger
    manix
    mpv
    mu
    niv
    nix-index
    nix-prefetch-git
    nixfmt
    nixpkgs-fmt
    nixpkgs-review
    nodePackages.bash-language-server
    nodePackages.javascript-typescript-langserver
    nodePackages.pyright
    nodejs
    python3
    ranger
    ripgrep
    rnix-lsp
    rustup
    shellcheck
    stow
    (texlive.combine {
      inherit (texlive) scheme-medium latexmk wrapfig rotfloat capt-of minted fvextra upquote catchfile xstring framed biblatex;
    })
    the-powder-toy
    tldr
    tmux
    tor
    tree
    unzip
    vim
    watch
    youtube-dl
    zathura
    zip
  ];
  darwinShellExtra = ''
    source $HOME/.nix-profile/etc/profile.d/nix.sh
  '';
  linuxShellExtra = "";
  sharedShellExtra = ''
    source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
  '';
  darwinShellAliases = {
    # useful command to run sequenced after a long command, `nix build; sd`
    sd = "say done";
    # brew bundle, but make it like home manager
    bb-check = "brew bundle check --global --verbose";
    bb-gc = "brew bundle cleanup --global --force";
    bb-switch = "brew bundle install --global --verbose";
    bb-upgrade = "brew bundle install --global --verbose --upgrade";
    # linuxkit-builder.  Unfortunately I'll have to install it
    # imperatively due to it lacking a bootstrap from darwin.
    lb-gc = "rm -rf ~/.cache/nix-linuxkit-builder/";
    lb-init = "nix-linuxkit-configure";
    lb-reset = "lb-stop; lb-gc; lb-init;";
    lb-start = "launchctl stop org.nix-community.linuxkit-builder";
    lb-stop = "launchctl stop org.nix-community.linuxkit-builder";
  };
in
{
  programs.home-manager.enable = true;

  home.username = "siraben";
  home.homeDirectory = if isDarwin then "/Users/siraben" else "/home/siraben";
  home.packages = sharedPackages ++ (lib.optionals isLinux linuxPackages)
    ++ (lib.optionals isDarwin darwinPackages);

  home.sessionVariables = { EDITOR = "emacsclient"; };
  home.file = lib.optionalAttrs isLinux {
    ".Xresources".text = ''
      Xft.dpi: 220
    '';
  };

  programs = {
    broot.enable = true;
    emacs.enable = isLinux;
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
      enableAutosuggestions = true;
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
        ne = "nix edit";
        nix-review-post = "nix-review pr --post-result";
        nr = "nix repl";
        nrp = "nix repl '<nixpkgs>'";
        ns = "nix-shell";
        tb = "tput bel";
      } // (lib.optionalAttrs isDarwin darwinShellAliases);
      initExtra = lib.concatStringsSep "\n"
        [
          (lib.optionalString isDarwin darwinShellExtra)
          (lib.optionalString isLinux linuxShellExtra)
          sharedShellExtra
        ];
    };
  };

  programs.direnv.enable = true;
  programs.direnv.enableNixDirenvIntegration = true;

  home.stateVersion = "20.09";
}
