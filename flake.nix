{
  description = "Siraben's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tree-sitter-sml = {
      url = "github:siraben/tree-sitter-sml";
      flake = false;
    };
    kitty-src = {
      url = "github:kovidgoyal/kitty";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, ... /* Capture all inputs */ }@allInputs:
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
          ];
        };

        server = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./server/configuration.nix
            ./server/hardware-configuration.nix
          ];
        };
      };

      homeConfigurations = {
        "${username}@macos-x86_64" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-darwin; # nixpkgs from output function args
          extraSpecialArgs = { inherit username; inputs = allInputs; minimal = false; };
          modules = [ ./home-manager/.config/nixpkgs/home.nix ];
        };
        "${username}@macos-aarch64" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;
          extraSpecialArgs = { inherit username; inputs = allInputs; minimal = false; };
          modules = [ ./home-manager/.config/nixpkgs/home.nix ];
        };
        "${username}@linux" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit username; inputs = allInputs; minimal = false; };
          modules = [ ./home-manager/.config/nixpkgs/home.nix ];
        };
        "${username}@minimal" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit username; inputs = allInputs; minimal = true; };
          modules = [ ./home-manager/.config/nixpkgs/home.nix ];
        };
      };

      devShells = nixpkgs.lib.genAttrs [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
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
