{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
  inputs.simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-21.11";

  outputs = { self, nixpkgs, simple-nixos-mailserver }: {
    nixosConfigurations.siraben-land = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        simple-nixos-mailserver.nixosModule
        ./configuration.nix
        { mailserver = import ./mailserver.nix {}; }
      ];
    };
    deploy.hosts = {
      siraben-land = {
        identity = "ObVbx/6AthjDGx1R28d8HQxypAyTKm8LUY5LZHotdIo";
        host = "45.77.156.171";
        user = "siraben";
      };
    };
  };
}
