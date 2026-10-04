{
  config,
  lib,
  pkgs,
  profile,
  ...
}:

let
  cfg = config.siraben.pi;
  enableCuaDriver = cfg.enableCuaDriver && pkgs.stdenv.hostPlatform.isDarwin;
  cuaDriverEnvironment = {
    CUA_DRIVER_RS_TELEMETRY_ENABLED = "0";
    CUA_DRIVER_RS_UPDATE_CHECK = "0";
  };
  piPackages = [
    "${pkgs.pi-better-background-tasks}/lib/node_modules/pi-better-background-tasks"
    "${pkgs.pi-subagents}/lib/node_modules/pi-subagents"
    "${pkgs.pi-codex-goal}/lib/node_modules/pi-codex-goal"
    "${pkgs.pi-tool-summaries}/lib/node_modules/pi-tool-summaries"
    "${pkgs.pi-web-access}/lib/node_modules/pi-web-access"
    {
      source = "${pkgs.context-mode}/lib/node_modules/context-mode";
      autoload = false;
    }
  ];
  piPackageSource = package: if builtins.isString package then package else package.source;
  subagentExtensions =
    map piPackageSource (
      builtins.filter (
        package:
        (package.autoload or true)
        && !(lib.hasInfix "-pi-better-background-tasks-" (piPackageSource package))
        && !(lib.hasInfix "-pi-subagents-" (piPackageSource package))
      ) piPackages
    )
    ++ map (name: "${config.home.homeDirectory}/${name}") (
      builtins.filter (lib.hasPrefix ".pi/agent/extensions/") (builtins.attrNames config.home.file)
    );
  declaredMcpServers = {
    # Codex's computer-use endpoint only accepts requests from signed clients.
    computer-use.enabled = false;
  }
  // lib.optionalAttrs enableCuaDriver {
    cua-driver = {
      command = lib.getExe pkgs.cua-driver;
      args = [ "mcp" ];
      env = cuaDriverEnvironment;
    };
  };
  writePiMcpConfig = pkgs.writers.writePython3 "write-pi-mcp-config" { } ''
    import json
    import os
    import tempfile
    import tomllib
    from pathlib import Path

    declared_path = Path(
        "${pkgs.writeText "pi-mcp-declared.json" (builtins.toJSON declaredMcpServers)}"
    )
    declared = json.loads(declared_path.read_text())
    home = Path.home()
    servers = {}

    codex = home / ".codex" / "config.toml"
    if ${if cfg.importCodexMcp then "True" else "False"} and codex.exists():
        codex_servers = tomllib.loads(codex.read_text()).get("mcp_servers", {})
        for name, entry in codex_servers.items():
            server = {}
            if "url" in entry:
                server["url"] = entry["url"]
                headers = dict(entry.get("http_headers") or {})
                environment_headers = entry.get("env_http_headers") or {}
                for header, variable in environment_headers.items():
                    headers[header] = "''${" + variable + "}"
                if entry.get("bearer_token_env_var"):
                    variable = entry["bearer_token_env_var"]
                    headers["Authorization"] = "Bearer ''${" + variable + "}"
                if headers:
                    server["headers"] = headers
            elif "command" in entry:
                server["command"] = entry["command"]
                for key in ("args", "env", "cwd"):
                    if entry.get(key):
                        server[key] = entry[key]
            else:
                continue

            timeout = entry.get("tool_timeout_sec")
            if timeout:
                server["timeout"] = timeout
            if entry.get("enabled") is False:
                server["enabled"] = False
            servers[name] = server

    for name, entry in declared.items():
        servers[name] = {**servers.get(name, {}), **entry}

    target = home / ".pi" / "agent" / "mcp.json"
    target.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    if target.is_symlink():
        target.unlink()

    fd, temporary_name = tempfile.mkstemp(
        dir=target.parent,
        prefix=f".{target.name}.",
        text=True,
    )
    try:
        os.fchmod(fd, 0o600)
        with os.fdopen(fd, "w") as output:
            json.dump({"mcpServers": servers}, output, indent=2)
            output.write("\n")
        os.replace(temporary_name, target)
    except BaseException:
        try:
            os.close(fd)
        except OSError:
            pass
        try:
            os.unlink(temporary_name)
        except FileNotFoundError:
            pass
        raise
  '';
in
{
  options.siraben.pi = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = profile != "minimal";
      description = "Whether to install and configure Pi";
    };
    enableCuaDriver = lib.mkEnableOption "the Cua Driver computer-use MCP server on macOS";
    importCodexMcp = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to import Codex MCP server declarations into Pi";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.pi
      pkgs.context-mode
    ]
    ++ lib.optional enableCuaDriver pkgs.cua-driver;

    home.sessionVariables = {
      PI_BETTER_BACKGROUND_TASKS_SHELL = lib.getExe pkgs.bash;
      PI_SKIP_VERSION_CHECK = "1";
      PI_TELEMETRY = "0";
    }
    // lib.optionalAttrs enableCuaDriver cuaDriverEnvironment;

    launchd.agents.cua-driver = lib.mkIf enableCuaDriver {
      enable = true;
      config = {
        ProgramArguments = [
          "${pkgs.cua-driver}/libexec/CuaDriver.app/Contents/MacOS/cua-driver"
          "serve"
        ];
        EnvironmentVariables = cuaDriverEnvironment;
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
      };
    };

    home.activation.writePiMcpConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${writePiMcpConfig}
    '';

    home.file = {
      ".pi/agent/themes/tomorrow-night-bright.json" = {
        force = true;
        source = ./pi-theme-tomorrow-night-bright.json;
      };

      ".pi/agent/settings.json" = {
        force = true;
        text = builtins.toJSON (
          {
            defaultModel = "gpt-5.6-sol";
            defaultProvider = "openai-codex";
            defaultThinkingLevel = "xhigh";
            modelThinkingLevels = {
              "openai-codex/gpt-5.6-sol" = "xhigh";
            };
            enableAnalytics = false;
            enableInstallTelemetry = false;
            lastChangelogVersion = pkgs.pi.version;
            theme = "tomorrow-night-bright";
            hideThinkingBlock = true;
            followUpMode = "all";
            toolSummaries = {
              model = "openrouter/openai/gpt-6-luna";
              reasoning = "off";
            };
            packages = piPackages;
            subagents.defaultExtensions = subagentExtensions;
          }
          // lib.optionalAttrs (lib.versionAtLeast pkgs.pi.version "1.0") {
            quietStartup = "header";
          }
        );
      };

      ".pi/agent/models.json" = {
        force = true;
        text = builtins.toJSON { providers = { }; };
      };
    };
  };
}
