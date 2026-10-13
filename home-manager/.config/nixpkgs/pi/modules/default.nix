{
  lib,
  pkgs,
  profile,
  ...
}:
let
  jsonFormat = pkgs.formats.json { };
  pathString = lib.types.str;
  packageEntry = lib.types.either pathString (
    lib.types.submodule {
      options = {
        source = lib.mkOption {
          type = pathString;
          description = "Pi package source or installed package directory";
        };
        autoload = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether Pi loads this package automatically";
        };
        extensions = lib.mkOption {
          type = lib.types.nullOr (lib.types.listOf lib.types.str);
          default = null;
          description = "Optional extension resource filters passed to Pi";
        };
        skills = lib.mkOption {
          type = lib.types.nullOr (lib.types.listOf lib.types.str);
          default = null;
          description = "Optional skill resource filters passed to Pi";
        };
        prompts = lib.mkOption {
          type = lib.types.nullOr (lib.types.listOf lib.types.str);
          default = null;
          description = "Optional prompt resource filters passed to Pi";
        };
        themes = lib.mkOption {
          type = lib.types.nullOr (lib.types.listOf lib.types.str);
          default = null;
          description = "Optional theme resource filters passed to Pi";
        };
        shareWithSubagents = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether to add this package explicitly to subagent extension defaults";
        };
      };
    }
  );
in
{
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
    settings = lib.mkOption {
      type = lib.types.attrsOf jsonFormat.type;
      default = { };
      description = "Pi settings merged recursively over the managed defaults";
    };
    packages = lib.mkOption {
      type = lib.types.listOf packageEntry;
      default = [ ];
      description = "Additional Pi packages appended to the configured package list";
    };
    extensions = lib.mkOption {
      type = lib.types.listOf pathString;
      default = [ ];
      description = "Additional standalone extension paths appended to Pi settings";
    };
    skills = lib.mkOption {
      type = lib.types.listOf pathString;
      default = [ ];
      description = "Additional skill paths appended to Pi settings";
    };
    prompts = lib.mkOption {
      type = lib.types.listOf pathString;
      default = [ ];
      description = "Additional prompt paths appended to Pi settings";
    };
    themes = lib.mkOption {
      type = lib.types.listOf pathString;
      default = [ ];
      description = "Additional theme paths appended to Pi settings";
    };
    instructions = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Additional global instructions appended to the bundled AGENTS.md";
    };
    subagentExtensions = lib.mkOption {
      type = lib.types.listOf pathString;
      default = [ ];
      description = "Standalone extensions explicitly shared with subagents";
    };
    providers = lib.mkOption {
      type = lib.types.attrsOf jsonFormat.type;
      default = { };
      description = "Custom model providers written to Pi's models.json";
    };
    mcpServers = lib.mkOption {
      type = lib.types.attrsOf jsonFormat.type;
      default = { };
      description = "Explicit MCP servers written to Pi's managed mcp.json";
    };
  };
}
