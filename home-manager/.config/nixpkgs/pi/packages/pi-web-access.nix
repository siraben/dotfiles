{
  buildPiNpmExtension,
  lib,
  nodejs,
  src,
}:
buildPiNpmExtension {
  pname = "pi-web-access";
  inherit src;
  npmLock = ./pi-web-access-package-lock.json;
  postPiInstallCheck = ''
    ${nodejs}/bin/node --check "$packageDir/dist/index.js"
  '';
  meta = {
    description = "Web search, fetching, PDF extraction, and video understanding for Pi";
    homepage = "https://github.com/nicobailon/pi-web-access";
    license = lib.licenses.mit;
  };
}
