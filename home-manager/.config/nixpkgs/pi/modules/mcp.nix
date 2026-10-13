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
in
{
  config = lib.mkIf cfg.enable {
    home.file.".pi/agent/mcp.json" = {
      force = true;
      text = builtins.toJSON { mcpServers = declaredMcpServers; };
    };
  };
}
