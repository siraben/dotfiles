{ pkgs, inputs }:
pkgs.stdenvNoCC.mkDerivation rec {
  pname = "pi-codex-goal";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;

  src = inputs.pi-codex-goal;

  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/node_modules/pi-codex-goal"
    cp -R . "$out/lib/node_modules/pi-codex-goal"

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Codex-style goal tracking and continuation for Pi";
    homepage = "https://github.com/fitchmultz/pi-codex-goal";
    license = licenses.mit;
  };
}
