{
  outputs = { self, nixpkgs }: {
    nixosConfigurations.siraben-land = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
      ];
    };
  };
}
