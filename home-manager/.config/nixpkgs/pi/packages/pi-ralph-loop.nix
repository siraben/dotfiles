{
  buildPiNpmExtension,
  lib,
  src,
}:
buildPiNpmExtension {
  pname = "pi-ralph-loop";
  packageName = "@lnilluv/pi-ralph-loop";
  inherit src;
  npmLock = ./pi-ralph-loop-package-lock.json;
  postPiInstallCheck = ''
    test -f "$packageDir/src/index.ts"
    node --input-type=module - "$packageDir" <<'JS'
      import { createRequire } from "node:module";
      const require = createRequire(process.argv[2] + "/package.json");
      require("yaml");
      require("minimatch");
    JS
  '';
  meta = {
    description = "Bounded autonomous coding loops with acceptance checks for Pi";
    homepage = "https://github.com/lnilluv/pi-ralph-loop";
    license = lib.licenses.mit;
  };
}
