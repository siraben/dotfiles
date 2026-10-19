{
  imports = [
    ./programs/git.nix
    ./programs/tmux.nix
    ./programs/kitty.nix
    ./programs/zsh.nix
    ./programs/direnv.nix
  ];

  programs = {
    gpg.enable = true;
    autojump.enable = true;
    mcfly.enable = true;
  };
}
