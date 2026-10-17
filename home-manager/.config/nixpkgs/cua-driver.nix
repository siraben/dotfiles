# Cua Driver package overlay.
final: _: {
  cua-driver = import ./pi/packages/cua-driver.nix { pkgs = final; };
}
