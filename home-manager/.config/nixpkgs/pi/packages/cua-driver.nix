# Preserve the notarized bundle verbatim; fixups would break its signature.
{ pkgs }:
pkgs.stdenvNoCC.mkDerivation rec {
  pname = "cua-driver";
  version = "0.30.4";

  src = pkgs.fetchurl {
    url = "https://github.com/trycua/cua/releases/download/cua-driver-rs-v${version}/cua-driver-rs-${version}-darwin-universal.tar.gz";
    hash = "sha256-nHWhhviTUvtSLcZ3kVdfjJ6AgaOHla8nBuED1B+nK+Q=";
  };

  dontConfigure = true;
  dontBuild = true;
  dontFixup = true;

  # Keep the bundle out of Applications/ so Home Manager does not add a
  # trampoline that could shadow it. The CLI decides whether it is running
  # inside the app from its resolved path, so link rather than wrap it.
  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/libexec
    cp -R CuaDriver.app $out/libexec/
    ln -s $out/libexec/CuaDriver.app/Contents/MacOS/cua-driver $out/bin/cua-driver

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Background computer-use driver and MCP server for macOS";
    homepage = "https://github.com/trycua/cua/tree/main/libs/cua-driver";
    changelog = "https://github.com/trycua/cua/releases/tag/cua-driver-rs-v${version}";
    license = licenses.mit;
    sourceProvenance = [ sourceTypes.binaryNativeCode ];
    platforms = platforms.darwin;
    mainProgram = "cua-driver";
  };
}
