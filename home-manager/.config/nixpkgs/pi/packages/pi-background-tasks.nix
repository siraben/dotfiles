{
  buildPiNpmExtension,
  fetchurl,
  importNpmLock,
  lib,
}:
let
  packageLock = lib.importJSON ./pi-background-tasks-package-lock.json;
in
buildPiNpmExtension rec {
  pname = "pi-background-tasks";
  version = "2.6.9";

  src = fetchurl {
    url = "https://registry.yarnpkg.com/pi-background-tasks/-/pi-background-tasks-${version}.tgz";
    hash = "sha512-5dma6xvO7jeAimRaCNCzfe5y+b4bVBYPxnJ/0RnGS+mj6B6SzgA4FcORKuAtZaoGvC29dlyof3NtX7/QD4Ywew==";
  };

  # The checked-in lock already contains integrity hashes for every dependency.
  npmDeps = importNpmLock {
    package = packageLock.packages."";
    inherit packageLock;
  };
  npmConfigHook = importNpmLock.npmConfigHook;

  npmFlags = [
    "--legacy-peer-deps"
    "--omit=dev"
  ];

  postPatch = ''
    sed -i '/  "devDependencies": {/,/^  },$/d' package.json
    cp ${./pi-background-tasks-package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib/node_modules/pi-background-tasks"
    cp -R . "$out/lib/node_modules/pi-background-tasks"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Durable background tasks, delegated agents, and multi-model Fusion workflows for Pi";
    homepage = "https://pi.dev/packages/pi-background-tasks";
    license = licenses.isc;
  };
}
