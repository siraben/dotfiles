{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.siraben.pi;
  configDir = config.programs.pi-coding-agent.configDir;
  packagePath = package: "${package}/lib/node_modules/${package.piPackageName}";

  bundledPackages = [
    {
      value = {
        source = "${pkgs.pi-background-tasks}/lib/node_modules/pi-background-tasks";
        # Exclude the separate Anthropic attribution entrypoint.
        extensions = [ "dist/extensions/background-tasks.js" ];
      };
      shareWithSubagents = false;
    }
    {
      value = packagePath pkgs.pi-subagents;
      shareWithSubagents = false;
    }
    {
      value = packagePath pkgs.pi-codex-goal;
      shareWithSubagents = true;
    }
    {
      value = packagePath pkgs.pi-tool-summaries;
      shareWithSubagents = true;
    }
    {
      value = packagePath pkgs.pi-web-access;
      shareWithSubagents = true;
    }
    {
      value = {
        source = packagePath pkgs.pi-context-mode;
        autoload = false;
      };
      shareWithSubagents = false;
    }
  ];

  packageSource =
    package:
    if builtins.isString package then
      package
    else if builtins.isAttrs package then
      package.source or ""
    else
      "";
  packageAutoload =
    package: builtins.isString package || (builtins.isAttrs package && (package.autoload or true));
  validPackage =
    package:
    builtins.isString package
    || (builtins.isAttrs package && package ? source && builtins.isString package.source);

  configuredBasePackages = cfg.settings.packages or (map (package: package.value) bundledPackages);
  configuredAutoloadSources = map packageSource (
    builtins.filter packageAutoload configuredBasePackages
  );

  normalizeExtraPackage =
    package:
    if builtins.isString package then
      {
        value = package;
        source = package;
        autoload = true;
        shareWithSubagents = false;
      }
    else
      {
        value = lib.filterAttrs (name: value: name != "shareWithSubagents" && value != null) package;
        inherit (package) source autoload shareWithSubagents;
      };
  extraPackages = map normalizeExtraPackage cfg.packages;
  piPackages = configuredBasePackages ++ map (package: package.value) extraPackages;

  bundledSubagentExtensions = map (package: packageSource package.value) (
    builtins.filter (
      package:
      package.shareWithSubagents && builtins.elem (packageSource package.value) configuredAutoloadSources
    ) bundledPackages
  );
  localSubagentExtensions = [
    "${configDir}/extensions/block-expensive-scans.ts"
    "${configDir}/extensions/codex-usage.ts"
  ];
  contributedSubagentExtensions = map (package: package.source) (
    builtins.filter (package: package.shareWithSubagents && package.autoload) extraPackages
  );
  defaultSubagentExtensions =
    bundledSubagentExtensions
    ++ localSubagentExtensions
    ++ contributedSubagentExtensions
    ++ cfg.subagentExtensions;

  managedDefaults = {
    defaultModel = "gpt-6.1-sol";
    defaultProvider = "openai-codex";
    defaultThinkingLevel = "medium";
    modelThinkingLevels = {
      "openai-codex/gpt-6.1-sol" = "medium";
    };
    enableAnalytics = false;
    enableInstallTelemetry = false;
    lastChangelogVersion = pkgs.pi.version;
    theme = "tomorrow-night-bright";
    hideThinkingBlock = true;
    showCacheMissNotices = true;
    followUpMode = "all";
    toolSummaries = {
      model = "openrouter/anthropic/claude-haiku-5.5";
      reasoning = "off";
      maxTokens = 1024;
    };
    packages = piPackages;
    subagents.defaultExtensions = defaultSubagentExtensions;
  }
  // lib.optionalAttrs (lib.versionAtLeast pkgs.pi.version "1.0") {
    quietStartup = "header";
  };

  mergedSettings = lib.recursiveUpdate managedDefaults cfg.settings;
  finalSettings =
    mergedSettings
    // {
      # Module package contributions append even when raw settings replace the
      # bundled package list. The two APIs therefore compose predictably.
      packages = piPackages;
    }
    // lib.optionalAttrs (cfg.extensions != [ ]) {
      extensions = (mergedSettings.extensions or [ ]) ++ cfg.extensions;
    }
    // lib.optionalAttrs (cfg.skills != [ ]) {
      skills = (mergedSettings.skills or [ ]) ++ cfg.skills;
    }
    // lib.optionalAttrs (cfg.prompts != [ ]) {
      prompts = (mergedSettings.prompts or [ ]) ++ cfg.prompts;
    }
    // lib.optionalAttrs (cfg.themes != [ ]) {
      themes = (mergedSettings.themes or [ ]) ++ cfg.themes;
    };
in
{
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          builtins.isList configuredBasePackages && builtins.all validPackage configuredBasePackages;
        message = "siraben.pi.settings.packages must be a list of strings or objects with a string source";
      }
      {
        assertion = !(cfg.settings ? extensions) || builtins.isList cfg.settings.extensions;
        message = "siraben.pi.settings.extensions must be a list when siraben.pi.extensions is used";
      }
      {
        assertion = !(cfg.settings ? skills) || builtins.isList cfg.settings.skills;
        message = "siraben.pi.settings.skills must be a list when siraben.pi.skills is used";
      }
      {
        assertion = !(cfg.settings ? prompts) || builtins.isList cfg.settings.prompts;
        message = "siraben.pi.settings.prompts must be a list when siraben.pi.prompts is used";
      }
      {
        assertion = !(cfg.settings ? themes) || builtins.isList cfg.settings.themes;
        message = "siraben.pi.settings.themes must be a list when siraben.pi.themes is used";
      }
    ];

    home.packages = [ pkgs.pi-context-mode ];
    home.sessionVariables = {
      PI_BG_DISABLE_PI_TELEMETRY = "1";
      PI_BG_DISABLE_UPDATE_CHECK = "1";
      # Keep delegation in pi-subagents rather than the process manager.
      PI_BG_FEATURES = "process,attribution";
      PI_SKIP_VERSION_CHECK = "1";
      PI_TELEMETRY = "0";
    };
    home.file."${configDir}/themes/tomorrow-night-bright.json" = {
      force = true;
      source = ../themes/tomorrow-night-bright.json;
    };

    # Keep the historical replacement behavior for files now owned upstream.
    home.file."${configDir}/settings.json".force = true;
    home.file."${configDir}/models.json".force = true;
    home.file."${configDir}/keybindings.json".force = true;

    programs.pi-coding-agent = {
      enable = true;
      package = pkgs.pi;
      models.providers = cfg.providers;
      # Emacs-style navigation; use M-e for the external editor.
      keybindings = {
        "app.model.cycleForward" = [ ];
        "app.model.cycleBackward" = [ ];
        "tui.editor.cursorUp" = [
          "up"
          "ctrl+p"
        ];
        "tui.editor.cursorDown" = [
          "down"
          "ctrl+n"
        ];
        "tui.select.up" = [
          "up"
          "ctrl+p"
        ];
        "tui.select.down" = [
          "down"
          "ctrl+n"
        ];
        "app.editor.external" = [ "alt+e" ];
      };
      settings = finalSettings;
    };
  };
}
