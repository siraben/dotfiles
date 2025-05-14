{
  description = "Siraben's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tree-sitter-cool = {
      url = "github:siraben/tree-sitter-cool";
      flake = false;
    };
    tree-sitter-formula = {
      url = "github:siraben/tree-sitter-formula";
      flake = false;
    };
    tree-sitter-promela = {
      url = "github:siraben/tree-sitter-promela";
      flake = false;
    };
    tree-sitter-sml = {
      url = "github:siraben/tree-sitter-sml";
      flake = false;
    };
    tree-sitter-typst = {
      url = "github:frozolotl/tree-sitter-typst";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, home-manager, ... /* Capture all inputs */ }@allInputs:
    let
      username = "siraben";
      # 'allInputs' is the attribute set of all defined flake inputs.
      # We will pass this set as 'inputs' within extraSpecialArgs.
    in
    {
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
          pkgs = nixpkgs.legacyPackages.${system}; # nixpkgs from output function args
        in
        {
          default = pkgs.mkShell {
            name = "home-manager-dotfiles-shell";
            packages = [
              allInputs.home-manager.packages.${system}.default # Use allInputs here
              pkgs.git
              # Add other common dev tools here if needed
            ];
            # shellHook for nix develop if required, but usually not for simple flake usage
            # For example, to make git aware of the .git-blame-ignore-revs file:
            # shellHook = ''
            #  git config blame.ignoreRevsFile .git-blame-ignore-revs
            # '';
          };
        });
    };
} 
