{
  description = "Siraben's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/b3582c75c7f21ce0b429898980eddbbf05c68e55";

    home-manager = {
      url = "github:nix-community/home-manager/1d2f0b3d4be0fed93c31d21603f988c68d5a4d16";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tree-sitter-cool = {
      url = "github:siraben/tree-sitter-cool/3bd8f928527c61575de056cf62b4fa0f79bd1fb6";
      flake = false;
    };
    tree-sitter-formula = {
      url = "github:siraben/tree-sitter-formula/351159cf66f0e7f8d86fa06fc44ab3c2055082df";
      flake = false;
    };
    tree-sitter-promela = {
      url = "github:siraben/tree-sitter-promela/91da8f141c3c4c695eb71018c8a7b2e7ea39c167";
      flake = false;
    };
    tree-sitter-sml = {
      url = "github:siraben/tree-sitter-sml/45699c3394c3c7741dda0d54fdb5af60f1ca3d25";
      flake = false;
    };
    tree-sitter-solidity = {
      url = "github:siraben/tree-sitter-solidity/f5fd341cc61613079e7bb513cd0db96982253743";
      flake = false;
    };
    tree-sitter-typst = {
      url = "github:frozolotl/tree-sitter-typst/4b935442f19cfdee7fd74800ed55a0f457f281a2";
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