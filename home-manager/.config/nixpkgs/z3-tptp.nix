{ z3-tptp }:

z3-tptp.overrideAttrs (oA: {
  installPhase = oA.installPhase + ''
    ln -s "z3_tptp5" "$out/bin/z3_tptp"
  '';
})
