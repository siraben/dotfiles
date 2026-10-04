final: _: {
  # nicobailon/pi-subagents: subagent delegation (foreground, parallel,
  # chains, detached background runs), built from the v0.74.0 source the
  # way upstream publishes it (`npm run build:pkg` -> dist-pkg).
  pi-subagents = final.buildNpmPackage rec {
    pname = "pi-subagents";
    version = "0.74.0";

    src = final.fetchFromGitHub {
      owner = "nicobailon";
      repo = "pi-subagents";
      # v0.74.0; the published tarball's gitHead.
      rev = "b6bda32f03b7f549623bc404c9be14dca298ddc4";
      hash = "sha256-QKf8Y8x90TLBKy3AkTRUc0+FxXAy/GKkHK2WQL2+a+k=";
    };

    patches = [
      # nicobailon/pi-subagents#2634, merged after 0.74.0: Pi 1.0's
      # pi-agent-core no longer exports "./node", which 0.74.0 required, so
      # every background/async subagent failed to launch. Source hunk only;
      # drop with the next release.
      ./pi-subagents-pi-1.0-agent-core-node.patch
    ];

    # Fetch the same npm tarballs through Yarn's registry mirror.
    npmDeps = final.fetchNpmDeps {
      inherit src patches;
      name = "${pname}-${version}-npm-deps";
      hash = "sha256-uwNrhoxNS6aa8PftDw4qWtGxKeVnGX4t/tcMoDgEwQY=";
      fetcherVersion = 2;
      npmRegistryOverridesString = builtins.toJSON {
        "registry.npmjs.org" = "https://registry.yarnpkg.com";
      };
    };

    npmDepsFetcherVersion = 2;
    npmFlags = [ "--ignore-scripts" ];
    npmBuildScript = "build:pkg";

    # build:pkg writes the publishable package to dist-pkg; install it with
    # only its runtime dependencies, as `npm install pi-subagents` would.
    installPhase = ''
      runHook preInstall

      npm prune --omit=dev --ignore-scripts --no-audit --no-fund --offline
      find node_modules -mindepth 1 -maxdepth 1 -type d -empty -delete
      cp -R node_modules dist-pkg/node_modules
      cd dist-pkg

      mkdir -p "$out/lib/node_modules/pi-subagents"
      cp -R . "$out/lib/node_modules/pi-subagents"

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
    version = "0.6.3";

    src = final.fetchurl {
      url = "https://registry.yarnpkg.com/pi-better-background-tasks/-/pi-better-background-tasks-${version}.tgz";
      hash = "sha512-11rJaj1NmE5tQU87vME5nEZJWFQfey1UMbNbKlMc7E93flKbYTd1gwuT017yyiQO9Nt1qR3LG2rkOp1qHWnOTQ==";
    };

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

  pi-background-tasks = final.buildNpmPackage rec {
    pname = "pi-background-tasks";
    version = "2.6.9";

    src = final.fetchurl {
      url = "https://registry.yarnpkg.com/pi-background-tasks/-/pi-background-tasks-${version}.tgz";
      hash = "sha512-5dma6xvO7jeAimRaCNCzfe5y+b4bVBYPxnJ/0RnGS+mj6B6SzgA4FcORKuAtZaoGvC29dlyof3NtX7/QD4Ywew==";
    };

    npmDepsHash = "sha256-GexsGFI2tHz7k9hupnYMdEKe/xm6eOufKCMPAwjk4oA=";

    npmFlags = [
      "--legacy-peer-deps"
      "--omit=dev"
    ];

    postPatch = ''
      sed -i '/  "devDependencies": {/,/^  },$/d' package.json
      cp ${./pi-background-tasks-package-lock.json} package-lock.json
    '';

    dontNpmBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib/node_modules/pi-background-tasks"
      cp -R . "$out/lib/node_modules/pi-background-tasks"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Durable background tasks, delegated agents, and multi-model Fusion workflows for Pi";
      homepage = "https://pi.dev/packages/pi-background-tasks";
      license = licenses.isc;
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
    version = "2.0.9";

    src = final.fetchurl {
      url = "https://registry.yarnpkg.com/@vanillagreen/pi-tool-renderer/-/pi-tool-renderer-${version}.tgz";
      hash = "sha512-l445hi9+PP6VKceI3lCTGqCt2MR8E3lJrNIsJDUJk8X7F/247gkYnkoMfvkiNHOhalAAAlK0x82GDVosRq+x7Q==";
    };

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
