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
  packageName = "context-mode";

  inherit src;

  npmLock = ./context-mode-package-lock.json;

  nativeBuildInputs = [
    makeBinaryWrapper
    pkg-config
    python3
  ];

  postInstall = ''
    # better-sqlite3's node-gyp output embeds Python, npm dependency, Node
    # source, and compiler paths. Keep only the ABI-specific runtime addon.
    native="$packageDir/node_modules/better-sqlite3"
    addon="$TMPDIR/better_sqlite3.node"
    cp "$native/build/Release/better_sqlite3.node" "$addon"
    rm -rf "$native/build" "$native/deps" "$native/src" "$native/binding.gyp"
    mkdir -p "$native/build/Release"
    cp "$addon" "$native/build/Release/better_sqlite3.node"

    mkdir -p "$out/bin"
    makeWrapper ${nodejs}/bin/node "$out/bin/context-mode" \
      --add-flags "$packageDir/cli.bundle.mjs"
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
  };
}
