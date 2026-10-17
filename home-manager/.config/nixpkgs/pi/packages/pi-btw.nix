{
  buildPiExtension,
  lib,
  src,
}:
buildPiExtension {
  pname = "pi-btw";
  inherit src;
  postPiInstallCheck = ''
    test -f "$packageDir/extensions/btw.ts"
    test -f "$packageDir/skills/btw/SKILL.md"
  '';
  meta = {
    description = "Parallel side conversations for Pi";
    homepage = "https://github.com/dbachelder/pi-btw";
    license = lib.licenses.mit;
  };
}
