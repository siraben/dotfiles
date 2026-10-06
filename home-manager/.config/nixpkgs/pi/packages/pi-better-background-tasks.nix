{ pkgs, inputs }:
pkgs.stdenvNoCC.mkDerivation rec {
  pname = "pi-better-background-tasks";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;
  src = inputs.pi-better-harness + "/packages/pi-better-background-tasks";

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/node_modules/pi-better-background-tasks"
    cp -R . "$out/lib/node_modules/pi-better-background-tasks"

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Durable background shell tasks, watchers, and logs for Pi";
    homepage = "https://github.com/1aboveio/pi-better-harness/tree/main/packages/pi-better-background-tasks";
    license = licenses.mit;
  };
}
