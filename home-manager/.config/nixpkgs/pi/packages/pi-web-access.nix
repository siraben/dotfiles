{ pkgs, inputs }:
pkgs.buildNpmPackage rec {
  pname = "pi-web-access";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;

  src = inputs.pi-web-access;

  npmDepsHash = "sha256-z34hqwPKL/aNGCD1A16B2y5HF7xNcVVVns5v43ldOTE=";

  npmFlags = [
    "--legacy-peer-deps"
    "--omit=dev"
  ];

  postPatch = ''
    sed -i '/  "devDependencies": {/,/^  },$/d' package.json
    cp ${./pi-web-access-package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/node_modules/pi-web-access"
    cp -R . "$out/lib/node_modules/pi-web-access"

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Web search, URL fetching, GitHub repo cloning, PDF extraction, and video understanding for Pi";
    homepage = "https://github.com/nicobailon/pi-web-access";
    license = licenses.mit;
  };
}
