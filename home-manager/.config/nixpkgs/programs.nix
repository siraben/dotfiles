{
  lib,
  pkgs,
  isDarwin,
  isLinux,
  profile,
}:

(import ./programs/git.nix { })
// (import ./programs/tmux.nix { })
// (import ./programs/kitty.nix { inherit pkgs isDarwin profile; })
// (import ./programs/zsh.nix {
  inherit
    lib
    pkgs
    isDarwin
    isLinux
    ;
})
// (import ./programs/direnv.nix { })
// {
  gpg.enable = true;
  autojump.enable = true;
  mcfly.enable = true;
}
