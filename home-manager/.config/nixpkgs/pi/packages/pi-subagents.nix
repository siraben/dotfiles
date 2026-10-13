{ pkgs, inputs }:
let
  subagentsSrc = inputs.pi-subagents;
  subagentsPackage = builtins.fromJSON (builtins.readFile "${subagentsSrc}/package.json");
  subagentsPackageLock = builtins.fromJSON (builtins.readFile "${subagentsSrc}/package-lock.json");
  subagentsBuildDependencies = subagentsPackage.dependencies // {
    inherit (subagentsPackage.devDependencies) typescript;
    "@types/node" = subagentsPackage.devDependencies."@types/node";
  };
  subagentsBuildPackage = removeAttrs subagentsPackage [ "devDependencies" ] // {
    dependencies = subagentsBuildDependencies;
  };
  subagentsBuildPackageLock = subagentsPackageLock // {
    packages =
      pkgs.lib.filterAttrs (
        name: package:
        !(package.dev or false)
        || builtins.elem name [
          "node_modules/typescript"
          "node_modules/@types/node"
          "node_modules/undici-types"
        ]
      ) subagentsPackageLock.packages
      // {
        "" = removeAttrs subagentsPackageLock.packages."" [ "devDependencies" ] // {
          dependencies = subagentsBuildDependencies;
        };
      };
  };
  subagentsBuildNodeModules = pkgs.importNpmLock.buildNodeModules {
    package = subagentsBuildPackage;
    packageLock = subagentsBuildPackageLock;
    nodejs = pkgs.nodejs;
  };
in
pkgs.stdenvNoCC.mkDerivation rec {
  pname = "pi-subagents";
  version = subagentsPackage.version;
  src = subagentsSrc;

  nativeBuildInputs = [ pkgs.nodejs ];

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

    cp -R ${subagentsBuildNodeModules}/node_modules .
    chmod -R u+w node_modules
    mkdir -p node_modules/@earendil-works
    for package in pi-agent-core pi-ai pi-coding-agent pi-tui; do
      ln -s ${pkgs.pi}/lib/pi/node_modules/@earendil-works/$package \
        node_modules/@earendil-works/$package
    done
    ln -s ${pkgs.pi}/lib/pi/node_modules/typebox node_modules/typebox
    npm run build:pkg --offline --ignore-scripts

    runHook postBuild
  '';

  # build:pkg writes the publishable package to dist-pkg. Install it with
  # only its runtime dependencies, as `npm install pi-subagents` would.
  installPhase = ''
    runHook preInstall

    rm -rf node_modules/.bin node_modules/@earendil-works node_modules/@types \
      node_modules/typebox node_modules/typescript node_modules/undici-types
    find node_modules -mindepth 1 -maxdepth 1 -type d -empty -delete
    cp -R node_modules dist-pkg/node_modules

    mkdir -p "$out/lib/node_modules/pi-subagents"
    cp -R dist-pkg/. "$out/lib/node_modules/pi-subagents"

    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Subagent delegation and multi-agent workflows for Pi";
    homepage = "https://github.com/nicobailon/pi-subagents";
    license = licenses.mit;
  };
}
