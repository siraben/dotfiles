{
  buildPiExtension,
  lib,
  src,
}:
let
  manifest = builtins.fromJSON (builtins.readFile "${src}/package.json");
in
buildPiExtension {
  pname = "pi-codex-goal";
  inherit (manifest) version;
  inherit src;

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    packageDir="$out/lib/node_modules/pi-codex-goal"
    mkdir -p "$packageDir"
    cp -R . "$packageDir"

    runHook postInstall
  '';

  meta = {
    description = "Codex-style goal tracking and continuation for Pi";
    homepage = "https://github.com/fitchmultz/pi-codex-goal";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
