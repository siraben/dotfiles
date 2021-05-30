{}:

{
  # useful command to run sequenced after a long command, `nix build; sd`
  sd = "say done";
  # brew bundle, but make it like home manager
  bb-check = "brew bundle check --global --verbose";
  bb-gc = "brew bundle cleanup --global --force";
  bb-switch = "brew bundle install --global --verbose";
  bb-upgrade = "brew bundle install --global --verbose --upgrade";
  linuxShell = ''docker run --rm -it lnl7/nix nix-shell -p nixFlakes --run "nix --experimental-features 'nix-command flakes' shell nixpkgs#nixUnstable"'';
}
