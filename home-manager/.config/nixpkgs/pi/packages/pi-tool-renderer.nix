{ pkgs, inputs }:
pkgs.stdenvNoCC.mkDerivation rec {
  pname = "pi-tool-renderer";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;
  src = inputs.kendex + "/pi-extensions/pi-tool-renderer";

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    rm -rf extensions/__tests__
    mkdir -p "$out/lib/node_modules/@vanillagreen/pi-tool-renderer"
    cp -R . "$out/lib/node_modules/@vanillagreen/pi-tool-renderer"

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Compact tool rows, diffs, and message rendering for Pi";
    homepage = "https://github.com/vanillagreencom/kendex/tree/main/pi-extensions/pi-tool-renderer";
    license = licenses.mit;
  };
}
