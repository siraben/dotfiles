{ pkgs }:
{
  mosh-unicode =
    pkgs.runCommand "mosh-unicode-regressions"
      {
        nativeBuildInputs = [
          pkgs.stdenv.cc
        ];
        buildInputs = [ pkgs.utf8proc ];
      }
      ''
          $CXX -std=c++11 -I${pkgs.mosh.src} \
            ${pkgs.mosh.src}/src/terminal/moshwcwidth.cc ${./widths.cc} \
            -lutf8proc -o widths
          ./widths > "$out"
        cat "$out"
      '';
  mosh-graphemes = pkgs.mosh.overrideAttrs (_: {
    doCheck = true;
    checkPhase = ''
      runHook preCheck
      make -C src/tests grapheme
      src/tests/grapheme
      runHook postCheck
    '';
  });
}
