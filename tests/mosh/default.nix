{ pkgs }:
{
  mosh-unicode =
    pkgs.runCommand "mosh-unicode-regressions"
      {
        nativeBuildInputs = [
          pkgs.stdenv.cc
          pkgs.python3
        ];
        buildInputs = [ pkgs.utf8proc ];
      }
      ''
          python3 ${./extract-legacy-width.py} \
            ${../../home-manager/.config/nixpkgs/mosh/unicode-16.patch} legacy.cc
          $CXX -std=c++11 -I${pkgs.mosh.src}/src/terminal \
            -Dmosh_wcwidth=legacy_mosh_wcwidth -c legacy.cc -o legacy.o
          $CXX -std=c++11 -I${pkgs.mosh.src} \
            ${pkgs.mosh.src}/src/terminal/moshwcwidth.cc ${./widths.cc} \
            legacy.o -lutf8proc -o widths
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
