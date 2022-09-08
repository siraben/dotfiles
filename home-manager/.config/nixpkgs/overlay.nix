final: prev:

{
  z3-tptp = prev.z3-tptp.overrideAttrs (oA: {
    installPhase = oA.installPhase + ''
    ln -s "z3_tptp5" "$out/bin/z3_tptp"
  '';
  });
  cool-osx = prev.callPackage ./cool.nix {};
}
