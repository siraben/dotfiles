{ pkgs, inputs }:
pkgs.stdenvNoCC.mkDerivation {
  pname = "pi-tool-summaries";
  version = "0.1.0-unstable-2026-10-03";

  src = inputs.pi-tool-summaries;

  dontBuild = true;

  # Install what the npm package would ship (package.json "files").
  installPhase = ''
    runHook preInstall

    dir="$out/lib/node_modules/pi-tool-summaries"
    mkdir -p "$dir"
    mkdir -p "$dir/docs"
    cp -R package.json README.md LICENSE src "$dir/"
    cp docs/before-after.svg "$dir/docs/"

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Plain-language summaries of Pi tool calls";
    homepage = "https://github.com/siraben/pi-tool-summaries";
    license = licenses.mit;
  };
}
