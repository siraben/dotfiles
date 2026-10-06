{
  lib,
  pkgs,
  isDarwin,
}:

{
  # Determinate Nix owns the system installation and configuration on macOS.
  # Enabling this module there puts upstream Nix in the activation PATH, which
  # warns about Determinate-only settings such as eval-cores and lazy-trees.
  nix.enable = !isDarwin;
  nix.package = lib.mkDefault pkgs.nix;
  nix.settings = {
    experimental-features = [
      "nix-command"
      "flakes"
    ];
    keep-derivations = true;
    keep-outputs = true;
    builders-use-substitutes = true;
    plugin-files = "";
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://siraben.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "siraben.cachix.org-1:/zSVUB18DWcjQF52VMh0v7MzjI+pdevnWOa01koPoYc="
    ];
  };
  xdg.configFile."nix/nix.conf" = lib.mkIf isDarwin {
    text = ''
      builders-use-substitutes = true
      experimental-features = nix-command flakes
      keep-derivations = true
      keep-outputs = true
      plugin-files =
      substituters = https://cache.nixos.org https://nix-community.cachix.org https://siraben.cachix.org
      trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= siraben.cachix.org-1:/zSVUB18DWcjQF52VMh0v7MzjI+pdevnWOa01koPoYc=
    '';
  };
}
