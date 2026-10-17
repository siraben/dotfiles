{
  buildPiExtension,
  lib,
  src,
}:
buildPiExtension {
  pname = "pi-tool-summaries";
  inherit src;
  meta = {
    description = "Plain-language summaries of Pi tool calls";
    homepage = "https://github.com/siraben/pi-tool-summaries";
    license = lib.licenses.mit;
  };
}
