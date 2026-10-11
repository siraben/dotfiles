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
    packages = lib.mkOption {
      type = lib.types.listOf (
        lib.types.either lib.types.str (
          lib.types.submodule {
            freeformType = lib.types.attrsOf lib.types.anything;
            options = {
              source = lib.mkOption {
                type = lib.types.str;
                description = "Pi package source or installed package directory";
              };
              autoload = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "Whether this package is loaded automatically";
              };
            };
          }
        )
      );
      default = [ ];
      description = "Additional Pi packages appended to the configured package list";
    };
    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Additional extension paths appended to Pi's resource settings";
    };
    skills = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Additional skill paths appended to Pi's resource settings";
    };
    instructions = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Additional global instructions appended to the bundled AGENTS.md";
    };
    subagentExtensions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Additional standalone extensions explicitly shared with subagents";
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
