{ inputs }:

final: prev:

{
  mosh-patched = prev.mosh.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./mosh/unicode-16.patch
    ];
  });
  mosh2 = prev.writeShellScriptBin "mosh2" ''
    exec ${final.mosh-patched}/bin/mosh "$@"
  '';
  pure-prompt = prev.pure-prompt.overrideAttrs (old: rec {
    version = "1.27.1";
    src = prev.fetchFromGitHub {
      owner = "sindresorhus";
      repo = "pure";
      rev = "v${version}";
      hash = "sha256-Fhk4nlVPS09oh0coLsBnjrKncQGE6cUEynzDO2Skiq8=";
    };
  });
  python3 = prev.python3.override {
    packageOverrides = pyFinal: pyPrev: {
      rapidfuzz = pyPrev.rapidfuzz.overridePythonAttrs (old: {
        postPatch = (old.postPatch or "") + prev.lib.optionalString prev.stdenv.isDarwin ''
          substituteInPlace CMakeLists.txt \
            --replace-fail "set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)" \
                           "set(CMAKE_INTERPROCEDURAL_OPTIMIZATION TRUE)
          set(CMAKE_CXX_SCAN_FOR_MODULES OFF)"
          substituteInPlace src/rapidfuzz/CMakeLists.txt \
            --replace-fail "if(NOT Windows)" \
                           "if(FALSE)"
        '';
      });
    };
  };
}
