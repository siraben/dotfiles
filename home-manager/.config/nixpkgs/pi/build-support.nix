{
  buildNpmPackage,
  lib,
  nodejs_22,
  stdenvNoCC,
}:

let
  extendPiExtension =
    finalAttrs:
    {
      pname,
      packageName ? pname,
      postPiInstallCheck ? "",
      nativeInstallCheckInputs ? [ ],
      doInstallCheck ? true,
      meta ? { },
      passthru ? { },
      ...
    }:
    {
      inherit doInstallCheck;
      strictDeps = true;

      nativeInstallCheckInputs = nativeInstallCheckInputs ++ [ nodejs_22 ];

      installCheckPhase = ''
        runHook preInstallCheck

        packageDir="$out/lib/node_modules/${packageName}"
        test -d "$packageDir"
        ${nodejs_22}/bin/node - "$packageDir/package.json" <<'JS'
          const manifest = require(process.argv[2]);
          const resources = ["extensions", "skills", "prompts", "themes"];
          if (!manifest.pi || !resources.some((name) => manifest.pi[name]?.length > 0)) {
            throw new Error("package does not declare any Pi resources");
          }
        JS
        ${postPiInstallCheck}

        runHook postInstallCheck
      '';

      passthru = passthru // {
        piPackageName = packageName;
      };

      meta = meta // {
        platforms = meta.platforms or nodejs_22.meta.platforms;
      };
    };

  extensionArgs = [
    "packageName"
    "postPiInstallCheck"
  ];
in
{
  buildPiExtension = lib.extendMkDerivation {
    constructDrv = stdenvNoCC.mkDerivation;
    excludeDrvArgNames = extensionArgs;
    extendDrvArgs = extendPiExtension;
  };

  buildPiNpmExtension = lib.extendMkDerivation {
    constructDrv = buildNpmPackage.override { nodejs = nodejs_22; };
    excludeDrvArgNames = extensionArgs;
    extendDrvArgs = extendPiExtension;
  };
}
