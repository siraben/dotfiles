# Keep local extension files available even in the minimal profile, as before.
{ ... }: {
  home.file = {
    # pi has no hooks.json; global extensions are auto-discovered here.
    ".pi/agent/extensions/block-expensive-scans.ts" = {
      force = true;
      source = ../../pi-block-expensive-scans.ts;
    };
    ".pi/agent/extensions/codex-usage.ts" = {
      force = true;
      source = ../../pi-codex-usage.ts;
    };
  };
}
