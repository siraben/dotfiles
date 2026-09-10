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
    agenix = {
      url = "github:ryantm/agenix";
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
      configurationName = "siraben";
      defaultUsername = "siraben";
      homeModule = ./home-manager/.config/nixpkgs/home.nix;
      mkHomeModule = profile: args@{ config, lib, pkgs, ... }:
        (import homeModule (args // { inherit profile; })) // {
          _module.args.profile = profile;
        };
      mkHomeConfiguration = {
        system,
        profile ? "full",
        username ? defaultUsername,
        timeZone ? "America/Los_Angeles",
        extraModules ? [],
        extraSpecialArgs ? {},
      }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          extraSpecialArgs = {
            inherit username profile timeZone;
            inputs = allInputs;
          } // extraSpecialArgs;
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
              home-manager.users.${defaultUsername} = homeModule;
              home-manager.extraSpecialArgs = {
                username = defaultUsername;
                inputs = allInputs;
                profile = "headless";
                timeZone = "Asia/Bangkok";
              };
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
            {
              # Surface the deployed Git revision in `nixos-version --json`.
              system.configurationRevision = self.rev or self.dirtyRev or null;
            }
          ];
        };
      };

      homeConfigurations = let
        darwinModules = [ mac-app-util.homeManagerModules.default ];
      in {
        # Darwin (full only; current Nixpkgs no longer supports Intel macOS)
        "${configurationName}@aarch64-darwin-full" = mkHomeConfiguration {
          system = "aarch64-darwin";
          profile = "full";
          extraModules = darwinModules;
        };

        # Linux x86_64
        "${configurationName}@x86_64-linux-full" = mkHomeConfiguration { system = "x86_64-linux"; profile = "full"; };
        "${configurationName}@x86_64-linux-headless" = mkHomeConfiguration { system = "x86_64-linux"; profile = "headless"; };
        "${configurationName}@x86_64-linux-minimal" = mkHomeConfiguration { system = "x86_64-linux"; profile = "minimal"; };

        # Linux aarch64
        "${configurationName}@aarch64-linux-headless" = mkHomeConfiguration { system = "aarch64-linux"; profile = "headless"; };
        "${configurationName}@aarch64-linux-minimal" = mkHomeConfiguration { system = "aarch64-linux"; profile = "minimal"; };
      };

      devShells = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ] (system:
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
