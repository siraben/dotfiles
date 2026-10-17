{
  buildNpmPackage,
  importNpmLock,
  lib,
  nodejs,
  stdenvNoCC,
}:
let
  extendPiExtension =
    finalAttrs:
    {
      pname,
      src,
      packageName ? pname,
      version ? (lib.importJSON "${src}/package.json").version,
      nativeBuildInputs ? [ ],
      postPiInstallCheck ? "",
      nativeInstallCheckInputs ? [ ],
      doInstallCheck ? true,
      meta ? { },
      passthru ? { },
      ...
    }:
    {
      inherit version doInstallCheck;
      strictDeps = true;
      dontBuild = true;
      nativeBuildInputs = nativeBuildInputs ++ [ nodejs ];
      nativeInstallCheckInputs = nativeInstallCheckInputs ++ [ nodejs ];

      # Let upstream's publish manifest select resources, including for a
      # TypeScript-only checkout. No lifecycle scripts or network are needed.
      installPhase = ''
        runHook preInstall
        packageDir="$out/lib/node_modules/${packageName}"
        mkdir -p "$packageDir"
        tarball="$(npm_config_cache="$TMPDIR/npm-pack-cache" \
          npm pack --ignore-scripts --silent --pack-destination "$TMPDIR")"
        tar -xzf "$TMPDIR/$tarball" -C "$packageDir" --strip-components=1
        if [ -d node_modules ]; then
          cp -R node_modules "$packageDir/node_modules"
          chmod -R u+w "$packageDir/node_modules"
          rm -rf "$packageDir/node_modules/.bin" "$packageDir/node_modules/.package-lock.json"
        fi
        find "$packageDir" -type f -name '*.map' -delete
        runHook postInstall
      '';

      installCheckPhase = ''
        runHook preInstallCheck
        packageDir="$out/lib/node_modules/${packageName}"
        ${nodejs}/bin/node - "$packageDir/package.json" <<'JS'
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
        piPackageRoot = "${finalAttrs.finalPackage}/lib/node_modules/${packageName}";
      };
      meta = meta // {
        platforms = meta.platforms or lib.platforms.unix;
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
    constructDrv = buildNpmPackage.override { inherit nodejs; };
    excludeDrvArgNames = extensionArgs ++ [ "npmLock" ];
    extendDrvArgs =
      finalAttrs:
      args@{
        npmLock,
        postPatch ? "",
        ...
      }:
      let
        lock = lib.importJSON npmLock;
      in
      extendPiExtension finalAttrs args
      // {
        npmDeps = importNpmLock {
          package = lock.packages."";
          packageLock = lock;
        };
        npmConfigHook = importNpmLock.npmConfigHook;
        npmFlags = [
          "--legacy-peer-deps"
          "--omit=dev"
        ];
        dontNpmBuild = true;
        # Match the root manifest to the managed lock without line-oriented
        # edits to JSON. Keep all runtime metadata from the published package.
        postPatch = ''
          ${nodejs}/bin/node - ${npmLock} <<'JS'
            const fs = require("node:fs");
            const manifest = JSON.parse(fs.readFileSync("package.json", "utf8"));
            const root = JSON.parse(fs.readFileSync(process.argv[2], "utf8")).packages[""];
            for (const key of ["dependencies", "devDependencies", "optionalDependencies", "peerDependencies", "peerDependenciesMeta"]) {
              delete manifest[key];
              if (root[key] !== undefined) manifest[key] = root[key];
            }
            fs.writeFileSync("package.json", JSON.stringify(manifest, null, 2) + "\n");
          JS
          cp ${npmLock} package-lock.json
        ''
        + postPatch;
      };
  };
}
