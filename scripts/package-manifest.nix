{ flakePath }:
let
  flake = builtins.getFlake ("path:" + flakePath);
  lib = flake.inputs.nixpkgs.lib;
  package = p: {
    name = p.pname or (builtins.parseDrvName p.name).name;
    version = p.version or (builtins.parseDrvName p.name).version;
  };
  homes = lib.mapAttrs' (name: home: lib.nameValuePair
    "Home Manager: ${name}" (map package home.config.home.packages)
  ) (flake.homeConfigurations or {});
  systems = lib.mapAttrs' (name: system: lib.nameValuePair
    "NixOS: ${name}" (map package (
      system.config.environment.systemPackages
      ++ [ system.config.boot.kernelPackages.kernel ]
      ++ system.config.fonts.packages
    ))
  ) (flake.nixosConfigurations or {});
  systemHomes = lib.foldlAttrs (acc: host: system: acc // (
    lib.mapAttrs' (user: home: lib.nameValuePair
      "Home Manager on ${host}: ${user}" (map package home.home.packages)
    ) (system.config.home-manager.users or {})
  )) {} (flake.nixosConfigurations or {});
in
homes // systems // systemHomes
