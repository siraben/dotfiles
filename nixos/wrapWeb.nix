{ lib, writeShellScriptBin, brave }:

name: url: writeShellScriptBin name ''
  ${brave}/bin/brave \
    --app=${lib.escapeShellArg url} \
    --user-data-dir=$HOME/.local/web-wrapper/${name}
''
