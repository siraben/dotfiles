{}:

{
  # useful command to run sequenced after a long command, `nix build; sd`
  sd = "say done";
  tailscale = "/Applications/Tailscale.app/Contents/MacOS/Tailscale";
  linuxShell = ''docker run --rm -it lnl7/nix nix-shell -p nixFlakes --run "nix --experimental-features 'nix-command flakes' shell nixpkgs#nixUnstable"'';
}
