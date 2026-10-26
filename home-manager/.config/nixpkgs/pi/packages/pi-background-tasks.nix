{
  buildPiNpmExtension,
  fetchurl,
  lib,
}:
buildPiNpmExtension rec {
  pname = "pi-background-tasks";
  version = "2.6.10";
  src = fetchurl {
    url = "https://registry.npmjs.org/pi-background-tasks/-/pi-background-tasks-2.6.10.tgz";
    hash = "sha512-ank/oCRst44FVCr4zwDvedzAxpf+Ttmm4XwB9SfKFJtvFWWX9T2YC+g2pWCdQnbGy/4X+BgoSr6YFCW+fWuhSw==";
  };
  npmLock = ./pi-background-tasks-package-lock.json;
  postPiInstallCheck = ''
    test -f "$packageDir/dist/extensions/background-tasks.js"
    node --check "$packageDir/dist/extensions/background-tasks.js"
  '';
  meta = {
    description = "Durable background tasks, delegated agents, and multi-model Fusion workflows for Pi";
    homepage = "https://pi.dev/packages/pi-background-tasks";
    license = lib.licenses.isc;
  };
}
