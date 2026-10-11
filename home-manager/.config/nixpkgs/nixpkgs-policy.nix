{ config, lib, ... }:
{
  options.siraben.allowedUnfreePackages = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [ ];
    description = "Unfree package names permitted in this Home Manager configuration";
  };

  config = {
    # A normal list definition lets other modules append names. Use mkForce to
    # replace the complete allowlist rather than extending it.
    siraben.allowedUnfreePackages = [
      "discord"
      "slack"
      "spotify"
      "spotify-unwrapped"
      "zoom"
      "aspell-dict-en-science"
      "claude-code"
      "context-mode"
    ];
    nixpkgs.config.allowUnfreePredicate =
      pkg: builtins.elem (lib.getName pkg) config.siraben.allowedUnfreePackages;
  };
}
