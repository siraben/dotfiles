{
  buildPiNpmExtension,
  lib,
  nodejs,
  src,
}:
buildPiNpmExtension {
  pname = "pi-subagents";
  inherit src;
  npmLock = ./pi-subagents-package-lock.json;
  postPiInstallCheck = ''
    ${nodejs}/bin/node --check "$packageDir/index.js"
  '';
  meta = {
    description = "Subagent delegation and multi-agent workflows for Pi";
    homepage = "https://github.com/nicobailon/pi-subagents";
    license = lib.licenses.mit;
  };
}
