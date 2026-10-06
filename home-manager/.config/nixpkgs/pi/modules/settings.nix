{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.siraben.pi;
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
in
{
  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.context-mode
    ];
    home.sessionVariables = {
      PI_BETTER_BACKGROUND_TASKS_SHELL = lib.getExe pkgs.bash;
      PI_SKIP_VERSION_CHECK = "1";
      PI_TELEMETRY = "0";
    };
    home.file = {
      ".pi/agent/themes/tomorrow-night-bright.json" = {
        force = true;
        source = ../themes/tomorrow-night-bright.json;
      };
    };
    # Keep the historical replacement behavior for files now owned upstream.
    home.file."${config.programs.pi-coding-agent.configDir}/settings.json".force = true;
    home.file."${config.programs.pi-coding-agent.configDir}/models.json".force = true;
    programs.pi-coding-agent = {
      enable = true;
      package = pkgs.pi;
      models.providers = cfg.providers;
      settings = lib.recursiveUpdate (
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
      ) cfg.settings;
    };
    # Companion resources and the managed MCP file use Pi's default directory.
    assertions = [
      {
        assertion = config.programs.pi-coding-agent.configDir == "${config.home.homeDirectory}/.pi/agent";
        message = "siraben.pi companion modules require the default Pi configDir (~/.pi/agent).";
      }
    ];
  };
}
