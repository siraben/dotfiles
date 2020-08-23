{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "siraben";
  home.homeDirectory = "/Users/siraben";
  home.packages = with pkgs; [
    ag
    borgbackup
    cabal-install
    chez
    ghc
    guile
    haskellPackages.haskell-language-server
    htop
    jq
    mu
    smlnj
    nodePackages.bash-language-server
    nodePackages.node2nix
    nodePackages.npm
    nodejs
    # python-language-server
    python37Packages.pillow 
    ranger
    rustup
    texlive.combined.scheme-small
    the-powder-toy
    tmux
    tor
    vim
    watch
    youtube-dl
    zile
    zathura
  ];
  
  programs = {
    git.enable = true;
    emacs.enable = true;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";
}
