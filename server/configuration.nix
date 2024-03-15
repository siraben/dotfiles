{ config, pkgs, ... }:

let
  publicKey = pkgs.fetchurl {
    url = "https://github.com/siraben.keys";
    sha256 = "sha256-i8gigitlRxiNa8eMPkD6FTOlVEO0p9LxRrBN41d/sJM=";
  };
in

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/vda";
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  networking.hostName = "siraben-land";

  networking.useDHCP = true;

  users.users.siraben = {
    shell = pkgs.zsh;
    useDefaultShell = false;
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  nix = {
    settings.trusted-users = [ "root" "siraben" ];
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
  services.openssh.settings.PasswordAuthentication = false;
  users.users.siraben.openssh.authorizedKeys.keyFiles = [ publicKey ];

  services.nginx = import ./nginx.nix { };

  security.acme = {
    acceptTerms = true;
    defaults.email = "bensiraphob@gmail.com";
  };

  security.sudo.wheelNeedsPassword = false;

  services.nextcloud = import ./nextcloud.nix { inherit pkgs; };

  services.postgresql = import ./postgresql.nix { };

  environment.pathsToLink = [ "/share/zsh" ];
  environment.shells = with pkgs; [ bashInteractive zsh ];

  programs.mosh.enable = true;
  programs.zsh.enable = true;
  services.tailscale.enable = true;
  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [ 80 443 ];
  system.stateVersion = "23.11";

}
