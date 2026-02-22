{ config, pkgs, lib, modulesPath, ... }:

let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILA4yYM7dggFdE6jy4/+LJf3icYiT6PxSboOHo+/zYad bensiraphob@gmail.com"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDGSUM4kaMhd0SaR7qXXbdUtTRy4uC9cPqbpfJ3QP+zZZfeU/rMg4Gv8w10JmFfvrPWFCRgZ3su7ewN+We3rbmN2qDxArOPdzjBfQ/N33epz1Th3fpswdoLmyYtUxeugqGo9TM2e4K4OwJwJnrOvfbfqqhkwvCYgcHzjsA2I1tEThI6eKcYInhq8IOSmwtnGNvl77HrH6cnXcrX3OK9XVSeHVzJKVwzb0IDsdr2fUdwCQZnlfeVj/LuXlDn5hueLQbi5qzyMdI+KeQA3i2iu+aq35Yn7ubOZjQ0kM0uCcm6nhWp4bXtSFGA4Kj4GwOvpTVULSdIW7mu6f37/OTW9MyuJnsFYxJgDUMB8giH4LHEOI9ZhYpkrvO01Lh2igCCVe8GGqDkpu9OQEzWRnFdE3oFH9QbPSWtniX2ZWH/zkoxP2iVGxJkcOiOZGAEsF19skyaCDyu0ZwC8xGzu8S6ZZic+BHeeXXstiquMuTemlU8dqxtmo+cw2xo7JqSZu20EPKjlXz/V6cVTfPQXeH+ANRz4bihdTfHEIEmAXH9PU4vni63loJvSdGqTITUtmQDpeSu+e5qF48IX+Hu+x+Hr/1HhGVn1o2G1DjutM+9BobHiMAq+rh/tPMc1zGlsdCyXf3121WBOrFG4fD/ZCdJoMAZzKaqmaIrcLvQbEVCrRHXNw== bensiraphob@gmail.com"
  ];
in
{
  imports = [
    "${modulesPath}/virtualisation/oci-common.nix"
    ./oci-hardware.nix
    ./zfs-mounts.nix
    ./minecraft.nix
  ];

  # Server tweaks
  zramSwap.enable = true;
  systemd.oomd.enable = true;
  boot.loader.grub.configurationLimit = 2;
  boot.tmp.cleanOnBoot = true;
  boot.tmp.useTmpfs = true;
  boot.kernelModules = [ "tcp_bbr" ];
  boot.kernel.sysctl = {
    "net.core.default_qdisc" = "fq";
    "net.ipv4.tcp_congestion_control" = "bbr";
    "net.ipv4.tcp_fastopen" = 3;
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  nix.optimise.automatic = true;
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 14d";
  };

  networking.hostName = "nixos-oci";

  # SSH
  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;
  users.users.root.openssh.authorizedKeys.keys = keys;

  # Nix
  nix = {
    settings.trusted-users = [ "root" "siraben" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Packages
  environment.systemPackages = with pkgs; [ vim git htop tmux ];

  # Shell & remote access
  programs.mosh.enable = true;
  programs.zsh.enable = true;
  services.tailscale.enable = true;
  networking.interfaces.tailscale0.mtu = 1280;
  services.eternal-terminal.enable = true;

  # User
  users.users.siraben = {
    shell = pkgs.zsh;
    isNormalUser = true;
    home = "/home/siraben";
    description = "Ben Siraphob";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = keys;
  };

  # On-demand Minecraft server
  age.secrets.minecraft-whitelist.file = ../secrets/minecraft-whitelist.age;
  services.ondemand-minecraft = {
    enable = true;
    environmentFiles = [ config.age.secrets.minecraft-whitelist.path ];
    whitelistFile = config.age.secrets.minecraft-whitelist.path;
    extraEnvironment = {
      ENFORCE_WHITELIST = "TRUE";
      VIEW_DISTANCE = "12";
    };
  };

  security.sudo.wheelNeedsPassword = false;

  # Disable wait-online (OCI networking is managed outside systemd-networkd)
  systemd.network.wait-online.enable = false;

  # Firewall (22=ssh, 2022=ET, 60000-61000=mosh)
  networking.firewall.allowedTCPPorts = [ 2022 ];
  networking.firewall.allowedUDPPorts = [ ];

  system.stateVersion = "25.11";
}
