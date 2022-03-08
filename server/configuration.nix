{ config, pkgs, ... }:

let
  publicKey = pkgs.fetchurl {
    url = "https://github.com/siraben.keys";
    sha256 = "sha256-hKkjnuGbhBjPv+V8XSRii/rOWTA4mL4rGIcu651wuR8=";
  };
in

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  networking.hostName = "siraben-land";

  networking.useDHCP = true;

  users.users.siraben = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  nix = {
    trustedUsers = [ "root" "siraben" ];
    package = pkgs.nixUnstable;
    optimise.automatic = true;
    gc = {
      automatic = true;
      dates = "weekly";
    };
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

  security.sudo.wheelNeedsPassword = false;

  services.nextcloud = import ./nextcloud.nix { inherit pkgs; };

  services.postgresql = import ./postgresql.nix { };
  # services.jitsi-meet = {
  #   enable = true;
  #   hostName = "meet.siraben.dev";
  # };
  # services.jitsi-videobridge.openFirewall = true;
  # services.prosody = {
  #   allowRegistration = true;
  #   admins = [ "siraben@siraben.dev" ];
  #   virtualHosts."meet.siraben.dev" = {
  #     enabled = true;
  #     extraConfig = ''
  #       authentication = "internal_hashed"
  #     '';
  #   };
  # };

  programs.mosh.enable = true;
  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 53 ];
  networking.firewall.allowedUDPPorts = [ 80 443 53 ];
  system.stateVersion = "21.05";

}
