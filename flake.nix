{
  description = "Siraben's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mac-app-util.url = "github:hraban/mac-app-util";
    tree-sitter-sml = {
      url = "github:siraben/tree-sitter-sml";
      flake = false;
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mosh-unicode = {
      url = "github:siraben/mosh/unicode";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, mac-app-util, ... /* Capture all inputs */ }@allInputs:
    let
      username = "siraben";
    in
    {
      # NixOS system configurations
      nixosConfigurations = {
        beelink = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./beelink/configuration.nix
            ./beelink/hardware-configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useUserPackages = true;
              home-manager.users.${username} = import ./home-manager/.config/nixpkgs/home.nix;
              home-manager.extraSpecialArgs = { inherit username; inputs = allInputs; profile = "headless"; };
            }
          ];
        };

        server = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./server/configuration.nix
            ./server/hardware-configuration.nix
          ];
        };

        server-arm = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            allInputs.agenix.nixosModules.default
            ./server-arm/configuration.nix
          ];
        };
      };

      homeConfigurations = let
        mkHome = { system, profile, extraModules ? [] }:
          home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.${system};
            extraSpecialArgs = { inherit username profile; inputs = allInputs; };
            modules = extraModules ++ [ ./home-manager/.config/nixpkgs/home.nix ];
          };
        darwinModules = [ mac-app-util.homeManagerModules.default ];
      in {
        # Darwin (full only)
        "${username}@x86_64-darwin-full"   = mkHome { system = "x86_64-darwin";  profile = "full"; extraModules = darwinModules; };
        "${username}@aarch64-darwin-full"   = mkHome { system = "aarch64-darwin"; profile = "full"; extraModules = darwinModules; };

        # Linux x86_64
        "${username}@x86_64-linux-full"     = mkHome { system = "x86_64-linux"; profile = "full"; };
        "${username}@x86_64-linux-headless" = mkHome { system = "x86_64-linux"; profile = "headless"; };
        "${username}@x86_64-linux-minimal"  = mkHome { system = "x86_64-linux"; profile = "minimal"; };

        # Linux aarch64
        "${username}@aarch64-linux-headless" = mkHome { system = "aarch64-linux"; profile = "headless"; };
        "${username}@aarch64-linux-minimal"  = mkHome { system = "aarch64-linux"; profile = "minimal"; };
      };

      devShells = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
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
        });
    };
}
