{ lib, writeShellScriptBin, ungoogled-chromium }:

name: url: writeShellScriptBin name ''
  ${ungoogled-chromium}/bin/chromium \
    --app=${lib.escapeShellArg url} \
    --user-data-dir=$HOME/.local/web-wrapper/${name}
''
