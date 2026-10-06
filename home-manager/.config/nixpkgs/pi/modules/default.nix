{ lib, profile, ... }: {
  imports = [
    ./settings.nix
    ./mcp.nix
    ./cua-driver.nix
    ./instructions.nix
    ./extensions.nix
  ];

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
    settings = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = "Pi settings merged recursively over the managed defaults";
    };
    providers = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = "Custom model providers written to Pi's models.json";
    };
    mcpServers = lib.mkOption {
      type = lib.types.attrsOf lib.types.anything;
      default = { };
      description = "MCP servers merged into Pi's generated mcp.json";
    };
  };
}
