{ config, lib, ... }:
let
  cfg = config.siraben.pi;
  configDir = config.programs.pi-coding-agent.configDir;
in
{
  config = lib.mkIf cfg.enable {
    home.file = {
      "${configDir}/skills/render-tex-pdf" = {
        force = true;
        source = ../../skills/render-tex-pdf;
      };
      # Pi has no hooks.json; global extensions are auto-discovered here.
      "${configDir}/extensions/block-expensive-scans.ts" = {
        force = true;
        source = ../../pi-block-expensive-scans.ts;
      };
      "${configDir}/extensions/codex-usage.ts" = {
        force = true;
        source = ../../pi-codex-usage.ts;
      };
    };
  };
}
