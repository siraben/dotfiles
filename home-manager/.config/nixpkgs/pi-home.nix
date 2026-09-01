{
  lib,
  pkgs,
  profile,
  ...
}:

let
  enablePi = profile != "minimal";
in
{
  home.packages = lib.optionals enablePi [
    pkgs.pi-coding-agent
    pkgs.context-mode
  ];

  home.file = {
    # Pi mutates settings at runtime, so keep the changelog marker aligned with
    # the packaged version and pin startup model selection declaratively.
    ".pi/agent/settings.json" = {
      force = true;
      text = builtins.toJSON {
        defaultModel = "gpt-5.6-sol";
        defaultProvider = "openai-codex";
        defaultThinkingLevel = "high";
        modelThinkingLevels = {
          "openai-codex/gpt-5.6-sol" = "high";
        };
        enableAnalytics = false;
        enableInstallTelemetry = false;
        lastChangelogVersion = pkgs.pi-coding-agent.version;
        theme = "dark";
        hideThinkingBlock = true;
        packages = lib.optionals enablePi [
          {
            source = "${pkgs.pi-background-tasks}/lib/node_modules/pi-background-tasks";
            extensions = [ "extensions/background-tasks.ts" ];
          }
          "${pkgs.pi-codex-goal}/lib/node_modules/pi-codex-goal"
          "${pkgs.pi-web-access}/lib/node_modules/pi-web-access"
          {
            source = "${pkgs.context-mode}/lib/node_modules/context-mode";
            autoload = false;
          }
        ];
      };
    };

    # Keep custom model declarations declarative and empty. Built-in/provider
    # model catalogs remain available; this specifically removes ox-alpha.
    ".pi/agent/models.json" = {
      force = true;
      text = builtins.toJSON { providers = { }; };
    };
  };
}
