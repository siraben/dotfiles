{ inputs }:

final: prev:

{
  mosh = prev.mosh.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [
      ./mosh/unicode-16.patch
    ];
  });
  codex = prev.codex.overrideAttrs (old: rec {
    version = "0.101.0-unstable-${inputs.codex-src.lastModifiedDate or "unknown"}";
    src = inputs.codex-src;
    sourceRoot = "source/codex-rs";
    postPatch = (old.postPatch or "") + ''
      substituteInPlace Cargo.toml \
        --replace-fail 'version = "0.0.0"' 'version = "${version}"'
    '';
    cargoDeps = prev.rustPlatform.importCargoLock {
      lockFile = "${inputs.codex-src}/codex-rs/Cargo.lock";
      outputHashes = {
        "crossterm-0.28.1" = "sha256-6qCtfSMuXACKFb9ATID39XyFDIEMFDmbx6SSmNe+728=";
        "nucleo-0.5.0" = "sha256-Hm4SxtTSBrcWpXrtSqeO0TACbUxq3gizg1zD/6Yw/sI=";
        "ratatui-0.29.0" = "sha256-HBvT5c8GsiCxMffNjJGLmHnvG77A6cqEL+1ARurBXho=";
        "runfiles-0.1.0" = "sha256-uJpVLcQh8wWZA3GPv9D8Nt43EOirajfDJ7eq/FB+tek=";
        "tokio-tungstenite-0.28.0" = "sha256-hJAkvWxDjB9A9GqansahWhTmj/ekcelslLUTtwqI7lw=";
        "tungstenite-0.27.0" = "sha256-AN5wql2X2yJnQ7lnDxpljNw0Jua40GtmT+w3wjER010=";
      };
    };
    buildInputs = (old.buildInputs or [ ]) ++ prev.lib.optionals prev.stdenv.hostPlatform.isLinux [
      prev.libcap
    ];
    doInstallCheck = false;
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
