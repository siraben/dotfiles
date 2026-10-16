{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.siraben.pi;
  piPackages = [
    {
      source = "${pkgs.pi-background-tasks}/lib/node_modules/pi-background-tasks";
      # Pi filters narrow the package manifest, which lists the compiled
      # entrypoints; skip its Anthropic attribution extension.
      extensions = [ "dist/extensions/background-tasks.js" ];
    }
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
        && !(lib.hasInfix "-pi-background-tasks-" (piPackageSource package))
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
      PI_BG_DISABLE_PI_TELEMETRY = "1";
      PI_BG_DISABLE_UPDATE_CHECK = "1";
      # Only process tasks; subagents cover delegation, so drop bg_delegate,
      # fusion_*, bg_run_pi_attested, and bg_result from the tool surface.
      PI_BG_FEATURES = "process,attribution";
      PI_SKIP_VERSION_CHECK = "1";
      PI_TELEMETRY = "0";
    };
    home.file = {
      ".pi/agent/themes/tomorrow-night-bright.json" = {
        force = true;
        source = ../themes/tomorrow-night-bright.json;
      };
    };
    home.file."${config.programs.pi-coding-agent.configDir}/keybindings.json".force = true;
    # Keep the historical replacement behavior for files now owned upstream.
    home.file."${config.programs.pi-coding-agent.configDir}/settings.json".force = true;
    home.file."${config.programs.pi-coding-agent.configDir}/models.json".force = true;
    programs.pi-coding-agent = {
      enable = true;
      package = pkgs.pi;
      models.providers = cfg.providers;
      # Emacs-style keys: C-p/C-n move and browse history instead of cycling
      # models, and C-g no longer opens the external editor (now M-e).
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
          showCacheMissNotices = true;
          followUpMode = "all";
          toolSummaries = {
            model = "openrouter/anthropic/claude-haiku-5.5";
            reasoning = "off";
            maxTokens = 1024;
          };
          packages = piPackages;
          subagents.defaultExtensions = subagentExtensions;
        }
        // lib.optionalAttrs (lib.versionAtLeast pkgs.pi.version "1.0") {
          quietStartup = "header";
        }
      ) cfg.settings;
    };
  };
}
