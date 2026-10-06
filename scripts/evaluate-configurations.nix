# Force every activation/system derivation, including non-native profiles.
# Home Manager outputs are not a standard output validated by flake check.
let
  flake = builtins.getFlake (toString ../.);
in
{
  home = builtins.mapAttrs (_: value: value.activationPackage.drvPath) (
    flake.homeConfigurations or { }
  );
  nixos = builtins.mapAttrs (_: value: value.config.system.build.toplevel.drvPath) (
    flake.nixosConfigurations or { }
  );
}
