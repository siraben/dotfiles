{
  description = "Beelink Mini S12 Pro NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      username = "siraben";
    in
    {
      nixosConfigurations = {
        beelink = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./configuration.nix
            ./hardware-configuration.nix

            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.siraben = import ../home-manager/.config/nixpkgs/home.nix;
              home-manager.extraSpecialArgs = { inherit username inputs; minimal = true; };
            }
          ];
        };
      };
    };
}