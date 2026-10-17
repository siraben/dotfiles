{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.siraben.pi;
  enableCuaDriver = cfg.enableCuaDriver && pkgs.stdenv.hostPlatform.isDarwin;
  declaredMcpServers =
    lib.optionalAttrs enableCuaDriver {
      cua-driver = {
        command = lib.getExe pkgs.cua-driver;
        args = [ "mcp" ];
        env = import ../cua-environment.nix;
      };
    }
    // cfg.mcpServers;
  mcpConfig = (pkgs.formats.json { }).generate "pi-mcp.json" {
    mcpServers = declaredMcpServers;
  };
in
{
  config = lib.mkIf cfg.enable {
    home.file."${config.programs.pi-coding-agent.configDir}/mcp.json" = {
      force = true;
      source = mcpConfig;
    };
  };
}
