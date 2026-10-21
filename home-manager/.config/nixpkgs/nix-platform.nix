{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  renderNixValue =
    value:
    if builtins.isBool value then
      lib.boolToString value
    else if builtins.isFloat value then
      lib.floatToString value
    else
      toString value;
  nixConf = lib.generators.toKeyValue {
    mkKeyValue = key: value: "${lib.escape [ "=" ] key} = ${renderNixValue value}";
  } config.nix.settings;
in
{
  config = {
    # Determinate Nix owns the system installation on macOS. The generated
    # fallback configuration applies the same settings without replacing it.
    nix.enable = lib.mkDefault (!isDarwin);
    nix.package = lib.mkDefault pkgs.nix;
    # Ad-hoc `nix shell nixpkgs#...` uses the same source as this generation.
    nix.registry.nixpkgs.flake = inputs.nixpkgs;
    nix.settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      keep-derivations = lib.mkDefault true;
      keep-outputs = lib.mkDefault true;
      builders-use-substitutes = lib.mkDefault true;
      plugin-files = lib.mkDefault "";
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
    xdg.configFile."nix/nix.conf" = lib.mkIf (isDarwin && !config.nix.enable) {
      text = nixConf + config.nix.extraOptions;
    };
    # HM gates its registry file on nix.enable too. Generate it ourselves when
    # Determinate owns Nix; the store-path reference also keeps nixpkgs GC-rooted.
    xdg.configFile."nix/registry.json" = lib.mkIf (isDarwin && !config.nix.enable) {
      source = (pkgs.formats.json { }).generate "registry.json" {
        version = config.nix.registryVersion;
        flakes = lib.mapAttrsToList (_: entry: {
          inherit (entry) from to exact;
        }) config.nix.registry;
      };
    };
  };
}
