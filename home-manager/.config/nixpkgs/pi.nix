final: _: {
  pi-coding-agent = final.buildNpmPackage rec {
    pname = "pi-coding-agent";
    version = "1.0.0";

    src = final.fetchurl {
      url = "https://github.com/earendil-works/pi/releases/download/v${version}/pi-${version}-source.tar.gz";
      hash = "sha256-iQicgtQXWbgAEkp34hKtqoZ8qqnRJp2AEu8N+byGuS4=";
    };

    # Fetch dependencies through Yarn's npm mirror. buildNpmPackage does not
    # forward registry overrides, so construct npmDeps explicitly.
    npmDeps = final.fetchNpmDeps {
      inherit src;
      name = "${pname}-${version}-npm-deps";
      hash = "sha256-ndEvWdB6sa5nNNtabk2OMZKUFG9x3op185deZHxFnXk=";
      npmRegistryOverridesString = builtins.toJSON {
        "registry.npmjs.org" = "https://registry.yarnpkg.com";
      };
    };

    npmWorkspace = "packages/coding-agent";

    npmRebuildFlags = [ "--ignore-scripts" ];

    nativeBuildInputs = [
      final.makeBinaryWrapper
    ];

    buildPhase = ''
      runHook preBuild

      npm run build --workspace=packages/chord
      npm run build --workspace=packages/tui
      npm run build --workspace=packages/telemetry
      npm run build --workspace=packages/codemode
      npm run build --workspace=packages/mcp
      npm run build:offline --workspace=packages/ai
      npm run build --workspace=packages/durable
      npm run build --workspace=packages/agent
      npm run build --workspace=packages/protocol
      npm run build --workspace=packages/client
      npm run build --workspace=packages/server
      npm run build --workspace=packages/coding-agent

      runHook postBuild
    '';

    postInstall = ''
      local nm="$out/lib/node_modules/pi-monorepo/node_modules"

      for ws in @earendil-works/chord:packages/chord \
                @earendil-works/pi-ai:packages/ai \
                @earendil-works/pi-agent-core:packages/agent \
                @earendil-works/pi-client:packages/client \
                @earendil-works/pi-codemode:packages/codemode \
                @earendil-works/pi-durable:packages/durable \
                @earendil-works/pi-mcp:packages/mcp \
                @earendil-works/pi-protocol:packages/protocol \
                @earendil-works/pi-server:packages/server \
                @earendil-works/pi-telemetry:packages/telemetry \
                @earendil-works/pi-tui:packages/tui; do
        IFS=: read -r pkg src <<< "$ws"
        rm "$nm/$pkg"
        cp -r "$src" "$nm/$pkg"
      done

      find "$nm" -type l -lname '*/packages/*' -delete
      find "$nm/.bin" -xtype l -delete
    ''
    + final.lib.optionalString final.stdenvNoCC.hostPlatform.isDarwin ''
      rm -rf \
        "$nm/@anthropic-ai/sandbox-runtime/dist/vendor/seccomp" \
        "$nm/@anthropic-ai/sandbox-runtime/vendor/seccomp"
    '';

    postFixup = ''
      wrapProgram $out/bin/pi --prefix PATH : ${
        final.lib.makeBinPath [
          final.ripgrep
          final.fd
        ]
      } \
        --set-default PI_BG_DISABLE_PI_TELEMETRY 1 \
        --set-default PI_BG_DISABLE_UPDATE_CHECK 1 \
        --set-default PI_SKIP_VERSION_CHECK 1 \
        --set-default PI_TELEMETRY 0
    '';

    meta = with final.lib; {
      description = "Coding agent CLI with read, bash, edit, write tools and session management";
      homepage = "https://pi.dev/";
      downloadPage = "https://www.npmjs.com/package/@earendil-works/pi-coding-agent";
      changelog = "https://github.com/earendil-works/pi/blob/main/packages/coding-agent/CHANGELOG.md";
      license = licenses.mit;
      mainProgram = "pi";
    };
  };

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

  pi-mcp-adapter = final.buildNpmPackage rec {
    pname = "pi-mcp-adapter";
    version = "2.33.0";

    src = final.fetchurl {
      url = "https://registry.yarnpkg.com/pi-mcp-adapter/-/pi-mcp-adapter-${version}.tgz";
      hash = "sha512-W1wFtd8NOz9+yAZZEoyEDfz4YMUxHSitPejZo4Yvol1YXQGeYiCfoFqd2k6GulP6k+w/p+L3NU2IcA/nlTkFEQ==";
    };

    npmDepsHash = "sha256-7IDVm4F+k/vAMA4LsMk1IkcANVKxCmeCXJmGMXud8E0=";

    npmFlags = [
      "--legacy-peer-deps"
      "--omit=dev"
    ];
    makeCacheWritable = true;

    nativeBuildInputs = [ final.makeBinaryWrapper ];

    postPatch = ''
      substituteInPlace package.json \
        --replace-fail \
          'https://pkg.pr.new/@modelcontextprotocol/core@3b205e7dd2f997b6a87e479e36421f7eaa2058e0' \
          'https://pkg.pr.new/modelcontextprotocol/typescript-sdk/@modelcontextprotocol/core@3b205e7'
      sed -i '/^  "devDependencies": {$/,$d' package.json
      sed -i '$s/,$//' package.json
      printf '}\n' >> package.json
      cp ${./pi-mcp-adapter-package-lock.json} package-lock.json
    '';

    dontNpmBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib/node_modules/pi-mcp-adapter" "$out/bin"
      cp -R . "$out/lib/node_modules/pi-mcp-adapter"
      makeWrapper ${final.nodejs}/bin/node "$out/bin/pi-mcp-adapter" \
        --add-flags "$out/lib/node_modules/pi-mcp-adapter/cli.js"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "MCP client extension and configuration adapter for Pi";
      homepage = "https://github.com/nicobailon/pi-mcp-adapter";
      license = licenses.mit;
      mainProgram = "pi-mcp-adapter";
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
