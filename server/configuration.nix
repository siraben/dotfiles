{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

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
  users.users.siraben.openssh.authorizedKeys.keyFiles = [ ./id_rsa.pub ];

  services.nginx = {
    enable = true;

    # Use recommended settings
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    # Only allow PFS-enabled ciphers with AES256
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";

    # Setup Nextcloud virtual host to listen on ports
    virtualHosts = {
      "cloud.siraben.dev" = {
        ## Force HTTP redirect to HTTPS
        forceSSL = true;
        ## LetsEncrypt
        enableACME = true;
      };
    };
  };

  security.acme = {
    acceptTerms = true;
    email = "bensiraphob@gmail.com";
  };

  services.nextcloud = import ./nextcloud.nix { inherit pkgs; };

  services.postgresql = import ./postgresql.nix { };

  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  system.stateVersion = "21.05";

}
