{ pkgs, ... }:

let
  keys = [''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDGSUM4kaMhd0SaR7qXXbdUtTRy4uC9cPqbpfJ3QP+zZZfeU/rMg4Gv8w10JmFfvrPWFCRgZ3su7ewN+We3rbmN2qDxArOPdzjBfQ/N33epz1Th3fpswdoLmyYtUxeugqGo9TM2e4K4OwJwJnrOvfbfqqhkwvCYgcHzjsA2I1tEThI6eKcYInhq8IOSmwtnGNvl77HrH6cnXcrX3OK9XVSeHVzJKVwzb0IDsdr2fUdwCQZnlfeVj/LuXlDn5hueLQbi5qzyMdI+KeQA3i2iu+aq35Yn7ubOZjQ0kM0uCcm6nhWp4bXtSFGA4Kj4GwOvpTVULSdIW7mu6f37/OTW9MyuJnsFYxJgDUMB8giH4LHEOI9ZhYpkrvO01Lh2igCCVe8GGqDkpu9OQEzWRnFdE3oFH9QbPSWtniX2ZWH/zkoxP2iVGxJkcOiOZGAEsF19skyaCDyu0ZwC8xGzu8S6ZZic+BHeeXXstiquMuTemlU8dqxtmo+cw2xo7JqSZu20EPKjlXz/V6cVTfPQXeH+ANRz4bihdTfHEIEmAXH9PU4vni63loJvSdGqTITUtmQDpeSu+e5qF48IX+Hu+x+Hr/1HhGVn1o2G1DjutM+9BobHiMAq+rh/tPMc1zGlsdCyXf3121WBOrFG4fD/ZCdJoMAZzKaqmaIrcLvQbEVCrRHXNw== bensiraphob@gmail.com'' ];
in

{
  imports = [
    ./hardware-configuration.nix
      "${fetchTarball {
    url = "https://github.com/onny/nixos-nextcloud-testumgebung/archive/fa6f062830b4bc3cedb9694c1dbf01d5fdf775ac.tar.gz";
    sha256 = "0gzd0276b8da3ykapgqks2zhsqdv4jjvbv97dsxg0hgrhb74z0fs";}}/nextcloud-extras.nix"
  ];

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;
  networking.hostName = "nixos";
  networking.domain = "subnet03111413.vcn03111413.oraclevcn.com";
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;
  users.users.root.openssh.authorizedKeys.keys = keys;


  nix = {
    settings.trusted-users = [ "root" "siraben" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = 1;

  environment.systemPackages = with pkgs; [ vim git htop ];
  programs.mosh.enable = true;
  programs.zsh.enable = true;
  services.tailscale.enable = true;

  users.users.siraben = {
      shell = pkgs.zsh;
      isNormalUser = true;
      home = "/home/siraben";
      description = "Ben Siraphob";
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = keys;
    };

  security.acme = {
    acceptTerms = true;
    defaults.email = "bensiraphob@gmail.com";
  };

  security.sudo.wheelNeedsPassword = false;

  services.nginx = import ./nginx.nix { };
  services.nextcloud = import ./nextcloud.nix { inherit pkgs; };
  services.postgresql = import ./postgresql.nix { };
  services.anki-sync-server = import ./anki-sync-server.nix { };

  systemd.services."nextcloud-setup" = {
    requires = ["postgresql.service"];
    after = ["postgresql.service"];
  };
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  networking.firewall.allowedUDPPorts = [ ];
  system.stateVersion = "24.11";
}
