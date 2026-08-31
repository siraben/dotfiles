{
  description = "Siraben's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
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
  };

  outputs = { self, nixpkgs, home-manager, mac-app-util, ... /* Capture all inputs */ }@allInputs:
    let
      username = "siraben";
    in
    {
      homeConfigurations = let
        mkHome = { system, profile, extraModules ? [] }:
          home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.${system};
            extraSpecialArgs = { inherit username profile; inputs = allInputs; };
            modules = extraModules ++ [ ./home-manager/.config/nixpkgs/home.nix ];
          };
        darwinModules = [ mac-app-util.homeManagerModules.default ];
      in {
        # Darwin (full only; current Nixpkgs no longer supports Intel macOS)
        "${username}@aarch64-darwin-full"   = mkHome { system = "aarch64-darwin"; profile = "full"; extraModules = darwinModules; };

        # Linux x86_64
        "${username}@x86_64-linux-full"     = mkHome { system = "x86_64-linux"; profile = "full"; };
        "${username}@x86_64-linux-headless" = mkHome { system = "x86_64-linux"; profile = "headless"; };
        "${username}@x86_64-linux-minimal"  = mkHome { system = "x86_64-linux"; profile = "minimal"; };

      };

      devShells = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-darwin" ] (system:
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
