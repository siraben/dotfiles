{ config, pkgs, ... }:

let
  publicKey = pkgs.fetchurl {
    url = "https://github.com/siraben.keys";
    sha256 = "sha256-i8gigitlRxiNa8eMPkD6FTOlVEO0p9LxRrBN41d/sJM=";
  };
  release = "master";
in

{
  imports = [
    ./hardware-configuration.nix
    (builtins.fetchTarball {
      url = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/${release}/nixos-mailserver-${release}.tar.gz";
      sha256 = "sha256:1a0g2zmqms0i98n6zavzw4cw2b9c2li6h06afkl42zngmgxl0k78";
    })
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  networking.hostName = "siraben-land";

  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;
  networking.interfaces.ens7.useDHCP = true;

  users.users.siraben = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  nix = {
    trustedUsers = [ "root" "siraben" ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  environment.systemPackages = with pkgs; [
    vim git tmux
  ];

  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  users.users.siraben.openssh.authorizedKeys.keyFiles = [ publicKey ];

  services.nginx = import ./nginx.nix { };

  security.acme = {
    acceptTerms = true;
    email = "bensiraphob@gmail.com";
  };

  services.nextcloud = import ./nextcloud.nix { inherit pkgs; };

  services.postgresql = import ./postgresql.nix { };

  mailserver = import ./mailserver.nix { };

  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  system.stateVersion = "21.05";

}
