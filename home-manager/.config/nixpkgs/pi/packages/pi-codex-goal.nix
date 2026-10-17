{
  buildPiExtension,
  lib,
  src,
}:
buildPiExtension {
  pname = "pi-codex-goal";
  inherit src;
  meta = {
    description = "Codex-style goal tracking and continuation for Pi";
    homepage = "https://github.com/fitchmultz/pi-codex-goal";
    license = lib.licenses.mit;
  };
}
