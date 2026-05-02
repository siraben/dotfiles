{ inputs }:

final: prev:

{
  # Workaround for NixOS/nix#15638 on darwin: Mach-O page-hash rewriting
  # invalidates ad-hoc signatures on zsh/fish, causing checkPhase scenarios to
  # hang or be SIGKILLed. Skip checks until the upstream fix lands.
  direnv = prev.direnv.overrideAttrs (_: { doCheck = false; doInstallCheck = false; });
  nix-direnv = prev.nix-direnv.overrideAttrs (_: { doCheck = false; doInstallCheck = false; });

  mosh = inputs.mosh-unicode.packages.${prev.system}.default;
  pure-prompt = prev.pure-prompt.overrideAttrs (old: rec {
    version = "1.27.1";
    src = prev.fetchFromGitHub {
      owner = "sindresorhus";
      repo = "pure";
      rev = "v${version}";
      hash = "sha256-Fhk4nlVPS09oh0coLsBnjrKncQGE6cUEynzDO2Skiq8=";
    };
  });
} // prev.lib.optionalAttrs prev.stdenv.isDarwin {
  python3 = prev.python3.override {
    packageOverrides = pyFinal: pyPrev: {
      rapidfuzz = pyPrev.rapidfuzz.overridePythonAttrs (old: {
        postPatch = (old.postPatch or "") + ''
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
