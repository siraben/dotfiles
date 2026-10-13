{
  buildPiNpmExtension,
  lib,
  nodejs_22,
  src,
}:

buildPiNpmExtension {
  pname = "pi-web-access";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;

  inherit src;

  npmDepsHash = "sha256-z34hqwPKL/aNGCD1A16B2y5HF7xNcVVVns5v43ldOTE=";
  npmFlags = [
    "--legacy-peer-deps"
    "--omit=dev"
  ];

  postPatch = ''
    sed -i '/  "devDependencies": {/,/^  },$/d' package.json
    cp ${./pi-web-access-package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall

    packageDir="$out/lib/node_modules/pi-web-access"
    mkdir -p "$packageDir"

    # npm pack applies the package manifest's files list. Avoid retaining the
    # dependency lock and npm build worktree in the runtime output.
    tarball="$(npm_config_cache="$TMPDIR/npm-pack-cache" \
      npm pack --ignore-scripts --silent --pack-destination "$TMPDIR")"
    tar -xzf "$TMPDIR/$tarball" -C "$packageDir" --strip-components=1
    cp -R node_modules "$packageDir/node_modules"
    chmod -R u+w "$packageDir/node_modules"
    rm -rf "$packageDir/node_modules/.bin" \
      "$packageDir/node_modules/.package-lock.json"
    find "$packageDir" -type f -name '*.map' -delete

    runHook postInstall
  '';

  postPiInstallCheck = ''
    ${nodejs_22}/bin/node --check "$packageDir/dist/index.js"
  '';

  meta = {
    description = "Web search, fetching, PDF extraction, and video understanding for Pi";
    homepage = "https://github.com/nicobailon/pi-web-access";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
