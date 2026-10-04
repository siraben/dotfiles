{ inputs }:
final: _:
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
    packages = final.lib.filterAttrs (
      name:
      package:
      !(package.dev or false)
      || builtins.elem name [
        "node_modules/typescript"
        "node_modules/@types/node"
        "node_modules/undici-types"
      ]
    ) subagentsPackageLock.packages // {
      "" = removeAttrs subagentsPackageLock.packages."" [ "devDependencies" ] // {
        dependencies = subagentsBuildDependencies;
      };
    };
  };
  subagentsBuildNodeModules = final.importNpmLock.buildNodeModules {
    package = subagentsBuildPackage;
    packageLock = subagentsBuildPackageLock;
    nodejs = final.nodejs;
  };
in
{
  # nicobailon/pi-subagents: subagent delegation (foreground, parallel,
  # chains, detached background runs). The flake lock selects the upstream
  # source, while importNpmLock avoids a separately maintained dependency hash.
  pi-subagents = final.stdenvNoCC.mkDerivation rec {
    pname = "pi-subagents";
    version = subagentsPackage.version;
    src = subagentsSrc;

    nativeBuildInputs = [ final.nodejs ];

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
        ln -s ${final.pi}/lib/pi/node_modules/@earendil-works/$package \
          node_modules/@earendil-works/$package
      done
      ln -s ${final.pi}/lib/pi/node_modules/typebox node_modules/typebox
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

    meta = with final.lib; {
      description = "Subagent delegation and multi-agent workflows for Pi";
      homepage = "https://github.com/nicobailon/pi-subagents";
      license = licenses.mit;
    };
  };

  # Plain-language, model-generated summaries of collapsed built-in Bash
  # calls (Ctrl+O shows the original call), configured through Pi's
  # settings.json `toolSummaries`. No runtime dependencies; Pi loads its
  # TypeScript sources directly. Not published to npm or tagged.
  pi-tool-summaries = final.stdenvNoCC.mkDerivation {
    pname = "pi-tool-summaries";
    version = "0.1.0-unstable-2026-10-03";

    src = final.fetchFromGitHub {
      owner = "siraben";
      repo = "pi-tool-summaries";
      rev = "bccfb8e20403b3f435dfc2c3b561d727ec891ae7";
      hash = "sha256-ZBdYqYUSt2NUSpqfy5vqciY6HMLA2tb9BmpCQISHVTI=";
    };

    dontBuild = true;

    # Install what the npm package would ship (package.json "files").
    installPhase = ''
      runHook preInstall

      dir="$out/lib/node_modules/pi-tool-summaries"
      mkdir -p "$dir"
      mkdir -p "$dir/docs"
      cp -R package.json README.md LICENSE src "$dir/"
      cp docs/before-after.svg "$dir/docs/"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Plain-language summaries of Pi tool calls";
      homepage = "https://github.com/siraben/pi-tool-summaries";
      license = licenses.mit;
    };
  };

  # Durable background shell tasks, watchers, logs, and completion wake-ups.
  # No runtime dependencies; Pi loads its TypeScript sources directly.
  pi-better-background-tasks = final.stdenvNoCC.mkDerivation rec {
    pname = "pi-better-background-tasks";
    version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;
    src = inputs.pi-better-harness + "/packages/pi-better-background-tasks";

    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib/node_modules/pi-better-background-tasks"
      cp -R . "$out/lib/node_modules/pi-better-background-tasks"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Durable background shell tasks, watchers, and logs for Pi";
      homepage = "https://github.com/1aboveio/pi-better-harness/tree/main/packages/pi-better-background-tasks";
      license = licenses.mit;
    };
  };

  pi-codex-goal = final.stdenvNoCC.mkDerivation rec {
    pname = "pi-codex-goal";
    version = "0.6.0";

    src = final.fetchurl {
      url = "https://registry.yarnpkg.com/pi-codex-goal/-/pi-codex-goal-${version}.tgz";
      hash = "sha512-Vfb257T/294pFwBObvUa4g+pF6k+Fl2dLALk0XnlF98EMJH0ra2J3iyrY1uhkQe0C++R7a3UFkqGTNkazzQRPQ==";
    };

    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib/node_modules/pi-codex-goal"
      cp -R . "$out/lib/node_modules/pi-codex-goal"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Codex-style goal tracking and continuation for Pi";
      homepage = "https://github.com/fitchmultz/pi-codex-goal";
      license = licenses.mit;
    };
  };

  # Compact tool-call, bash, and MCP rendering for Pi >= 0.99. No runtime
  # dependencies; the tarball's __tests__ directory (bun:test) is not loaded.
  pi-tool-renderer = final.stdenvNoCC.mkDerivation rec {
    pname = "pi-tool-renderer";
    version = (builtins.fromJSON (builtins.readFile "${src}/package.json")).version;
    src = inputs.kendex + "/pi-extensions/pi-tool-renderer";

    dontBuild = true;

    installPhase = ''
      runHook preInstall

      rm -rf extensions/__tests__
      mkdir -p "$out/lib/node_modules/@vanillagreen/pi-tool-renderer"
      cp -R . "$out/lib/node_modules/@vanillagreen/pi-tool-renderer"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Compact tool rows, diffs, and message rendering for Pi";
      homepage = "https://github.com/vanillagreencom/kendex/tree/main/pi-extensions/pi-tool-renderer";
      license = licenses.mit;
    };
  };

  pi-web-access = final.buildNpmPackage rec {
    pname = "pi-web-access";
    version = "0.35.0";

    src = final.fetchurl {
      url = "https://registry.yarnpkg.com/pi-web-access/-/pi-web-access-${version}.tgz";
      hash = "sha512-D1GMJMNDG2omAkL1ztmO6ZEwEyW7LoFpTcckKh2yXjLyAgDFuhpTQrs2jQPAlVtGb3rjAeIXRZrkhFcwSWdGSg==";
    };

    npmDepsHash = "sha256-/1K73mLU5O0/vfObOvaGz/97TVij14uknn8Q994joTw=";

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

      mkdir -p "$out/lib/node_modules/pi-web-access"
      cp -R . "$out/lib/node_modules/pi-web-access"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Web search, URL fetching, GitHub repo cloning, PDF extraction, and video understanding for Pi";
      homepage = "https://github.com/nicobailon/pi-web-access";
      license = licenses.mit;
    };
  };

  context-mode = final.buildNpmPackage rec {
    pname = "context-mode";
    version = "1.0.169";

    src = final.fetchurl {
      url = "https://registry.yarnpkg.com/context-mode/-/context-mode-${version}.tgz";
      hash = "sha512-94JIaFuLjF9SO2BsGTrbGtyT44K95+9OC8BdbaL/UT76xOkanJLfUR5CzmNw+GELXZQqH4nBrKg9wjBnSFkVnQ==";
    };

    npmDepsHash = "sha256-OvXBWsGeDKGRyt71mLJJ6P03GQeGq9VTgHZQQ+8SK8s=";

    npmFlags = [
      "--legacy-peer-deps"
      "--omit=dev"
    ];

    nativeBuildInputs = [
      final.makeBinaryWrapper
      final.python3
      final.pkg-config
    ];

    postPatch = ''
      cp ${./context-mode-package-lock.json} package-lock.json
    '';

    dontNpmBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib/node_modules/context-mode" "$out/bin"
      cp -R . "$out/lib/node_modules/context-mode"
      chmod +x "$out/lib/node_modules/context-mode/cli.bundle.mjs"
      makeWrapper "$out/lib/node_modules/context-mode/cli.bundle.mjs" "$out/bin/context-mode" \
        --prefix PATH : ${final.lib.makeBinPath [ final.nodejs ]}

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Token-efficient context management for coding agents";
      homepage = "https://pi.dev/packages/context-mode";
      license = licenses.elastic20;
      mainProgram = "context-mode";
    };
  };
}
