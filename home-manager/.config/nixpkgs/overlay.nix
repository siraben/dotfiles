final: prev:

{
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
