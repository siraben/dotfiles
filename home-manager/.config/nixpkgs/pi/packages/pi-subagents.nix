{
  buildPiExtension,
  importNpmLock,
  lib,
  nodejs,
  runtime,
  src,
}:
let
  package = builtins.fromJSON (builtins.readFile "${src}/package.json");
  packageLock = builtins.fromJSON (builtins.readFile "${src}/package-lock.json");
  buildDependencies = package.dependencies // {
    inherit (package.devDependencies) typescript;
    "@types/node" = package.devDependencies."@types/node";
  };
  buildPackage = removeAttrs package [ "devDependencies" ] // {
    dependencies = buildDependencies;
  };
  buildPackageLock = packageLock // {
    packages =
      lib.filterAttrs (
        name: dependency:
        !(dependency.dev or false)
        || builtins.elem name [
          "node_modules/typescript"
          "node_modules/@types/node"
          "node_modules/undici-types"
        ]
      ) packageLock.packages
      // {
        "" = removeAttrs packageLock.packages."" [ "devDependencies" ] // {
          dependencies = buildDependencies;
        };
      };
  };
  buildNodeModules = importNpmLock.buildNodeModules {
    package = buildPackage;
    packageLock = buildPackageLock;
    inherit nodejs;
  };
in
buildPiExtension {
  pname = "pi-subagents";
  inherit (package) version;
  inherit src;

  nativeBuildInputs = [ nodejs ];

  postPatch = ''
    if grep -Fq 'steer: (text) => session.steer(text),' src/runs/shared/child-session.ts; then
      substituteInPlace src/runs/shared/child-session.ts \
        --replace-fail 'steer: (text) => session.steer(text),' \
          'steer: async (text) => { await session.steer(text); },' \
        --replace-fail 'followUp: (text) => session.followUp(text),' \
          'followUp: async (text) => { await session.followUp(text); },'
    fi
  '';

  buildPhase = ''
    runHook preBuild

    cp -R ${buildNodeModules}/node_modules .
    chmod -R u+w node_modules
    mkdir -p node_modules/@earendil-works
    for packageName in pi-agent-core pi-ai pi-coding-agent pi-tui; do
      ln -s ${runtime.sdkRoot}/@earendil-works/$packageName \
        node_modules/@earendil-works/$packageName
    done
    ln -s ${runtime.sdkRoot}/typebox node_modules/typebox
    npm run build:pkg --offline --ignore-scripts

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    rm -rf node_modules/.bin node_modules/.package-lock.json \
      node_modules/@earendil-works node_modules/@types \
      node_modules/typebox node_modules/typescript node_modules/undici-types
    find node_modules -mindepth 1 -maxdepth 1 -type d -empty -delete
    cp -R node_modules dist-pkg/node_modules

    packageDir="$out/lib/node_modules/pi-subagents"
    mkdir -p "$packageDir"
    cp -R dist-pkg/. "$packageDir"
    find "$packageDir" -type f -name '*.map' -delete

    runHook postInstall
  '';

  postPiInstallCheck = ''
    ${nodejs}/bin/node --check "$packageDir/index.js"
  '';

  meta = {
    description = "Subagent delegation and multi-agent workflows for Pi";
    homepage = "https://github.com/nicobailon/pi-subagents";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
