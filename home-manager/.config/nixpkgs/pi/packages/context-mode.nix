{ pkgs, inputs }:
pkgs.buildNpmPackage rec {
  pname = "context-mode";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;

  src = inputs.context-mode;

  npmDepsHash = "sha256-OvXBWsGeDKGRyt71mLJJ6P03GQeGq9VTgHZQQ+8SK8s=";

  npmFlags = [
    "--legacy-peer-deps"
    "--omit=dev"
  ];

  nativeBuildInputs = [
    pkgs.makeBinaryWrapper
    pkgs.python3
    pkgs.pkg-config
  ];

  postPatch = ''
    cp ${./context-mode-package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/node_modules/context-mode" "$out/bin"
    cp -R . "$out/lib/node_modules/context-mode"
    chmod +x "$out/lib/node_modules/context-mode/cli.bundle.mjs"
    makeWrapper "$out/lib/node_modules/context-mode/cli.bundle.mjs" "$out/bin/context-mode" \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.nodejs ]}

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Token-efficient context management for coding agents";
    homepage = "https://pi.dev/packages/context-mode";
    license = licenses.elastic20;
    mainProgram = "context-mode";
  };
}
