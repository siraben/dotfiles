final: prev:

{
  kitty = prev.kitty.overrideAttrs (_old: let
    version = "unstable-2025-10-31";
    src = prev.fetchFromGitHub {
      owner = "kovidgoyal";
      repo = "kitty";
      rev = "6d50da1f486c447dfd6cbfcbe27b74b56a243820";
      hash = "sha256-OjiXBr+E3+/VgUwI0LuZ16X0mMl1WHqOMIB/lfRpfNo=";
    };
    goModules = (prev.buildGo124Module {
      pname = "kitty-go-modules";
      inherit src version;
      vendorHash = "sha256-mALG466wGE8YYwQ9wxXrN9r6DsuW+iopIbH7RHNiMKc=";
    }).goModules;
  in {
    inherit version src goModules;
  });
}
