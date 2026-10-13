{
  buildPiExtension,
  lib,
  src,
}:
let
  manifest = builtins.fromJSON (builtins.readFile "${src}/package.json");
  sourceDate = src.lastModifiedDate or "19700101000000";
  date = "${builtins.substring 0 4 sourceDate}-${builtins.substring 4 2 sourceDate}-${
    builtins.substring 6 2 sourceDate
  }";
in
buildPiExtension {
  pname = "pi-tool-summaries";
  version = "${manifest.version}-unstable-${date}";
  inherit src;

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    packageDir="$out/lib/node_modules/pi-tool-summaries"
    mkdir -p "$packageDir/docs"
    cp -R package.json README.md LICENSE src "$packageDir/"
    cp docs/before-after.svg "$packageDir/docs/"

    runHook postInstall
  '';

  meta = {
    description = "Plain-language summaries of Pi tool calls";
    homepage = "https://github.com/siraben/pi-tool-summaries";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
