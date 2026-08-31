final: _: {
  pi-coding-agent = final.buildNpmPackage rec {
    pname = "pi-coding-agent";
    version = "0.84.4";

    src = final.fetchurl {
      url = "https://github.com/earendil-works/pi/releases/download/v${version}/pi-${version}-source.tar.gz";
      hash = "sha256-yjlYVZtg+H7kTITZTfjD7gt+2ldTcEAquy0K2RVc3ko=";
    };

    npmDepsHash = "sha256-35GC3Q4Jf4URvqoEYHeM63x49tTmrth62//PvKm4I7Q=";

    npmWorkspace = "packages/coding-agent";

    npmRebuildFlags = [ "--ignore-scripts" ];

    nativeBuildInputs = [
      final.makeBinaryWrapper
    ];

    buildPhase = ''
      runHook preBuild

      npm run build --workspace=packages/tui
      npm run build --workspace=packages/telemetry
      npm run build:offline --workspace=packages/ai
      npm run build --workspace=packages/agent
      npm run build --workspace=packages/protocol
      npm run build --workspace=packages/client
      npm run build --workspace=packages/coding-agent

      runHook postBuild
    '';

    postInstall = ''
      local nm="$out/lib/node_modules/pi-monorepo/node_modules"

      for ws in @earendil-works/pi-ai:packages/ai \
                @earendil-works/pi-agent-core:packages/agent \
                @earendil-works/pi-client:packages/client \
                @earendil-works/pi-protocol:packages/protocol \
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

  pi-subagents = final.buildNpmPackage rec {
    pname = "pi-subagents";
    version = "0.14.3";

    src = final.fetchurl {
      url = "https://registry.npmjs.org/@tintinweb/pi-subagents/-/pi-subagents-${version}.tgz";
      hash = "sha512-iDqeadh6114AZvw8HYe1PEq8M0MZ9czJKTAIsklCPUbV9vUMS+g/LAV0vW3O9PiBXKNJh8hkrY8L6iIr8XNEqA==";
    };

    npmDepsHash = "sha256-8J0iPuc4h6mBqfRopmU180+3b/I5JT1ucJVXFvyBapk=";

    npmFlags = [
      "--legacy-peer-deps"
      "--omit=dev"
    ];

    postPatch = ''
      cp ${./pi-subagents-package-lock.json} package-lock.json
    '';

    dontNpmBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/lib/node_modules/@tintinweb/pi-subagents"
      cp -R . "$out/lib/node_modules/@tintinweb/pi-subagents"

      runHook postInstall
    '';

    meta = with final.lib; {
      description = "Claude Code-style autonomous sub-agents extension for Pi";
      homepage = "https://github.com/tintinweb/pi-subagents";
      license = licenses.mit;
    };
  };

  pi-background-tasks = final.buildNpmPackage rec {
    pname = "pi-background-tasks";
    version = "2.4.2";

    src = final.fetchurl {
      url = "https://registry.npmjs.org/pi-background-tasks/-/pi-background-tasks-${version}.tgz";
      hash = "sha512-KDH2yv5yKnc2slUNMSsysVZleriuv8tbhe5L+AeplVAfijQsECN5YAWOz5TDbStCXLdJC15GaUQ1P87BXGk5Hg==";
    };

    npmDepsHash = "sha256-smu7glMF90IrlyoTi9dGtGT7PBQYY/OAKCLT183L+Qg=";

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
    version = "0.2.0";

    src = final.fetchurl {
      url = "https://registry.npmjs.org/pi-codex-goal/-/pi-codex-goal-${version}.tgz";
      hash = "sha512-NCL7WJ1wLwMyiTlKlc9sTTTZdTQzSS2HJcuth3PYD8YWDjt9eVpNQdAGVW/sz7UjvCCpzlD9dAg7yRIt9H3t7g==";
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

  pi-web-access = final.buildNpmPackage rec {
    pname = "pi-web-access";
    version = "0.27.0";

    src = final.fetchurl {
      url = "https://registry.npmjs.org/pi-web-access/-/pi-web-access-${version}.tgz";
      hash = "sha512-D/z7ILwbnJeDjzFPC1j3G1OvO+j2vl2H13ByYcH5FLbrJ1yBdbBwTBcl96Bbt2NEqH5vdmoZ/EpbDG8BTF9W7Q==";
    };

    npmDepsHash = "sha256-O92tw/9oRbitdGzr0ZTmtTnCNtgwB4fkA5ZO/N1rnwQ=";

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
      url = "https://registry.npmjs.org/context-mode/-/context-mode-${version}.tgz";
      hash = "sha512-94JIaFuLjF9SO2BsGTrbGtyT44K95+9OC8BdbaL/UT76xOkanJLfUR5CzmNw+GELXZQqH4nBrKg9wjBnSFkVnQ==";
    };

    npmDepsHash = "sha256-jwCimDVJXiCVQ2oWZMKoZtwi8DA3rB8KqvJe9C8eudA=";

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
      license = licenses.mit;
      mainProgram = "context-mode";
    };
  };
}
