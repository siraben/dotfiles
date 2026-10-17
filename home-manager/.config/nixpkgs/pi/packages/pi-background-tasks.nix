{
  buildPiNpmExtension,
  fetchurl,
  lib,
}:
buildPiNpmExtension rec {
  pname = "pi-background-tasks";
  version = "2.6.9";
  src = fetchurl {
    url = "https://registry.npmjs.org/pi-background-tasks/-/pi-background-tasks-${version}.tgz";
    hash = "sha512-5dma6xvO7jeAimRaCNCzfe5y+b4bVBYPxnJ/0RnGS+mj6B6SzgA4FcORKuAtZaoGvC29dlyof3NtX7/QD4Ywew==";
  };
  npmLock = ./pi-background-tasks-package-lock.json;
  meta = {
    description = "Durable background tasks, delegated agents, and multi-model Fusion workflows for Pi";
    homepage = "https://pi.dev/packages/pi-background-tasks";
    license = lib.licenses.isc;
  };
}
