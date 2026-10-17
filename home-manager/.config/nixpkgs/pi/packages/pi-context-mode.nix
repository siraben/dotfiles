{
  buildPiNpmExtension,
  lib,
  makeBinaryWrapper,
  nodejs,
  pkg-config,
  python3,
  src,
}:

buildPiNpmExtension {
  pname = "pi-context-mode";
  version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;
  packageName = "context-mode";

  inherit src;

  npmDepsHash = "sha256-OvXBWsGeDKGRyt71mLJJ6P03GQeGq9VTgHZQQ+8SK8s=";
  npmFlags = [
    "--legacy-peer-deps"
    "--omit=dev"
  ];

  nativeBuildInputs = [
    makeBinaryWrapper
    pkg-config
    python3
  ];

  postPatch = ''
    cp ${./context-mode-package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall

    packageDir="$out/lib/node_modules/context-mode"
    mkdir -p "$packageDir" "$out/bin"

    # Install the same file set as the published npm package rather than the
    # npm build worktree. Runtime dependencies are copied separately below.
    tarball="$(npm_config_cache="$TMPDIR/npm-pack-cache" \
      npm pack --ignore-scripts --silent --pack-destination "$TMPDIR")"
    tar -xzf "$TMPDIR/$tarball" -C "$packageDir" --strip-components=1
    cp -R node_modules "$packageDir/node_modules"
    chmod -R u+w "$packageDir/node_modules"
    rm -rf "$packageDir/node_modules/.bin" \
      "$packageDir/node_modules/.package-lock.json"

    # better-sqlite3's node-gyp output embeds Python, npm dependency, Node
    # source, and compiler paths. Keep only the ABI-specific runtime addon.
    native="$packageDir/node_modules/better-sqlite3"
    addon="$TMPDIR/better_sqlite3.node"
    cp "$native/build/Release/better_sqlite3.node" "$addon"
    rm -rf "$native/build" "$native/deps" "$native/src" "$native/binding.gyp"
    mkdir -p "$native/build/Release"
    cp "$addon" "$native/build/Release/better_sqlite3.node"

    find "$packageDir" -type f -name '*.map' -delete

    makeWrapper ${nodejs}/bin/node "$out/bin/context-mode" \
      --add-flags "$packageDir/cli.bundle.mjs"

    runHook postInstall
  '';

  postPiInstallCheck = ''
    ${nodejs}/bin/node -e \
      'require(process.argv[1]);' \
      "$packageDir/node_modules/better-sqlite3/build/Release/better_sqlite3.node"

    export HOME="$TMPDIR/home"
    mkdir -p "$HOME"
    ${nodejs}/bin/node --input-type=module - "$packageDir" "$TMPDIR/session.db" <<'JS'
      const [packageDir, dbPath] = process.argv.slice(2);
      const { SessionDB } = await import(`file://''${packageDir}/build/session/db.js`);
      const db = new SessionDB({ dbPath });
      db.close();
      await import(`file://''${packageDir}/build/adapters/pi/extension.js`);
    JS
  '';

  meta = {
    description = "Token-efficient context management package and Pi adapter";
    homepage = "https://github.com/mksglu/context-mode";
    license = lib.licenses.elastic20;
    mainProgram = "context-mode";
    platforms = lib.platforms.unix;
  };
}
