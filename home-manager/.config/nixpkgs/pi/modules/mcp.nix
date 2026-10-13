{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.siraben.pi;
  enableCuaDriver = cfg.enableCuaDriver && pkgs.stdenv.hostPlatform.isDarwin;
  declaredMcpServers = {
    # Preserve the existing signed-client endpoint override.
    computer-use.enabled = false;
  }
  // lib.optionalAttrs enableCuaDriver {
    cua-driver = {
      command = lib.getExe pkgs.cua-driver;
      args = [ "mcp" ];
      env = import ../cua-environment.nix;
    };
  }
  // cfg.mcpServers;
  declared = pkgs.writeText "pi-mcp-declared.json" (builtins.toJSON declaredMcpServers);
  writer = pkgs.writers.writePython3 "write-pi-mcp-config" { } (
    builtins.readFile ./write-mcp-config.py
  );
in
{
  config = lib.mkIf cfg.enable {
    home.activation.writePiMcpConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${writer} ${declared} ${lib.boolToString cfg.importCodexMcp}
    '';
  };
}
