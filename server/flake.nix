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
  };
}
