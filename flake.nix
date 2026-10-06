{
  description = "siraben's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pi = {
      url = "github:earendil-works/pi/stable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pi-subagents = {
      url = "github:nicobailon/pi-subagents";
      flake = false;
    };
    pi-better-harness = {
      url = "github:1aboveio/pi-better-harness";
      flake = false;
    };
    kendex = {
      url = "github:vanillagreencom/kendex";
      flake = false;
    };
    pi-tool-summaries = {
      url = "github:siraben/pi-tool-summaries";
      flake = false;
    };
    # Published bundles preserve the existing runtime artifacts. Update these
    # release URLs together with packages/*-package-lock.json and npmDepsHash.
    pi-codex-goal = {
      url = "https://registry.npmjs.org/pi-codex-goal/-/pi-codex-goal-0.6.0.tgz";
      flake = false;
    };
    pi-web-access = {
      url = "https://registry.npmjs.org/pi-web-access/-/pi-web-access-0.35.0.tgz";
      flake = false;
    };
    context-mode = {
      url = "https://registry.npmjs.org/context-mode/-/context-mode-1.0.169.tgz";
      flake = false;
    };
    mac-app-util = {
      url = "github:siraben/mac-app-util-py";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mosh-unicode = {
      url = "github:siraben/mosh/unicode";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    siraben-overlay = {
      url = "github:siraben/overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      mac-app-util,
      ... # Capture all inputs
    }@allInputs:
    let
      configurationName = "siraben";
      defaultUsername = "siraben";
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      homeModule = ./home-manager/.config/nixpkgs/home.nix;
      mkHomeModule =
        profile:
        args@{
          config,
          lib,
          pkgs,
          ...
        }:
        (import homeModule (args // { inherit profile; }))
        // {
          _module.args.profile = profile;
        };
      mkHomeConfiguration =
        {
          system,
          profile ? "full",
          username ? defaultUsername,
          timeZone ? "America/New_York",
          extraModules ? [ ],
          extraSpecialArgs ? { },
        }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          extraSpecialArgs = {
            inherit username profile timeZone;
            inputs = allInputs;
          }
          // extraSpecialArgs;
          modules = [ homeModule ] ++ extraModules;
        };
    in
    {
      # Public extension points for wrapper flakes and host-specific overlays.
      homeManagerModules = {
        default = mkHomeModule "full";
        full = mkHomeModule "full";
        headless = mkHomeModule "headless";
        minimal = mkHomeModule "minimal";
      };

      lib.mkHomeConfiguration = mkHomeConfiguration;

      homeConfigurations =
        let
          darwinModules = [ mac-app-util.homeManagerModules.default ];
        in
        {
          # Darwin (full only; current Nixpkgs no longer supports Intel macOS)
          "${configurationName}@aarch64-darwin-full" = mkHomeConfiguration {
            system = "aarch64-darwin";
            profile = "full";
            extraModules = darwinModules;
          };

          # Linux x86_64
          "${configurationName}@x86_64-linux-full" = mkHomeConfiguration {
            system = "x86_64-linux";
            profile = "full";
          };
          "${configurationName}@x86_64-linux-headless" = mkHomeConfiguration {
            system = "x86_64-linux";
            profile = "headless";
          };
          "${configurationName}@x86_64-linux-minimal" = mkHomeConfiguration {
            system = "x86_64-linux";
            profile = "minimal";
          };

          # Linux aarch64
          "${configurationName}@aarch64-linux-headless" = mkHomeConfiguration {
            system = "aarch64-linux";
            profile = "headless";
          };
          "${configurationName}@aarch64-linux-minimal" = mkHomeConfiguration {
            system = "aarch64-linux";
            profile = "minimal";
          };
        };

      devShells = nixpkgs.lib.genAttrs supportedSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            name = "home-manager-dotfiles-shell";
            packages = [
              allInputs.home-manager.packages.${system}.default
              pkgs.git
            ];
          };
        }
      );

      checks = nixpkgs.lib.genAttrs supportedSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          pi-mcp-config =
            pkgs.runCommand "pi-mcp-config-tests"
              {
                nativeBuildInputs = [ pkgs.python3 ];
              }
              ''
                mkdir -p pi/modules pi/tests
                cp ${./home-manager/.config/nixpkgs/pi/modules/write-mcp-config.py} pi/modules/write-mcp-config.py
                cp ${./home-manager/.config/nixpkgs/pi/tests/test_mcp_config.py} pi/tests/test_mcp_config.py
                python3 -B -m unittest discover -s pi/tests
                touch "$out"
              '';
        }
      );

      formatter = nixpkgs.lib.genAttrs supportedSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
    };
}
