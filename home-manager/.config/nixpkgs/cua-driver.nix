# Compatibility overlay for the original package entry point.
final: _: {
  cua-driver = import ./pi/packages/cua-driver.nix { pkgs = final; };
}
