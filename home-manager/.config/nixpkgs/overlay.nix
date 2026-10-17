{ inputs }:

final: prev: {

  # Apply the local tmux-window UI patch to Agent Deck from llm-agents.nix.
  agent-deck = prev.llm-agents.agent-deck.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./agent-deck-hide-window-rows.patch ];
    # The patch intentionally changes behavior covered by upstream UI tests.
    doCheck = false;
  });

  # NixOS/nix#15638 invalidates ad-hoc zsh/fish signatures during Darwin
  # Mach-O page-hash rewriting, so their check phases hang or receive SIGKILL.
  direnv = prev.direnv.overrideAttrs (_: { doCheck = false; doInstallCheck = false; });
  nix-direnv = prev.nix-direnv.overrideAttrs (_: { doCheck = false; doInstallCheck = false; });

  mosh = inputs.mosh-unicode.packages.${prev.stdenv.hostPlatform.system}.default;
} // prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
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
