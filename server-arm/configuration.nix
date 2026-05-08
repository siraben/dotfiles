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
    ./soju.nix
    ./wastebin.nix
    "${fetchTarball {
      url = "https://github.com/onny/nixos-nextcloud-testumgebung/archive/fa6f062830b4bc3cedb9694c1dbf01d5fdf775ac.tar.gz";
      sha256 = "0gzd0276b8da3ykapgqks2zhsqdv4jjvbv97dsxg0hgrhb74z0fs";
    }}/nextcloud-extras.nix"
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
    # Bigger TCP buffers for the 25Gb Mellanox NIC + cross-region BDP
    "net.core.rmem_max" = 33554432;
    "net.core.wmem_max" = 16777216;
    "net.core.rmem_default" = 1048576;
    "net.core.wmem_default" = 1048576;
    # tcp_rmem max stays at 32 MiB to avoid regressing kernel default; tcp_wmem max
    # at 16 MiB matches the wmem_max we actually need for outbound 1 Gbps egress.
    "net.ipv4.tcp_rmem" = "4096 1048576 33554432";
    "net.ipv4.tcp_wmem" = "4096 1048576 16777216";
    "net.core.netdev_max_backlog" = 5000;
    "net.ipv4.tcp_mtu_probing" = 1;
    # zramSwap is enabled, so prefer cache over swap; retain dentries longer
    "vm.swappiness" = 10;
    "vm.vfs_cache_pressure" = 50;
  };
  # Cap the ZFS ARC at 8 GiB. Without this it grows to ~50% RAM (~12 GiB)
  # and on this box has been observed creeping toward the c_max default.
  # Capping leaves more headroom for postgres + minecraft JVM + bots.
  boot.extraModprobeConfig = ''
    options zfs zfs_arc_max=8589934592
  '';
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

  # Secrets
  age.secrets.minecraft-whitelist.file = ../secrets/minecraft-whitelist.age;
  age.secrets.nextcloud-admin-pass = { file = ../secrets/nextcloud-admin-pass.age; owner = "nextcloud"; };
  age.secrets.nextcloud-siraben-pass = { file = ../secrets/nextcloud-siraben-pass.age; owner = "nextcloud"; };
  age.secrets.anki-sync-pass.file = ../secrets/anki-sync-pass.age;
  age.secrets.cloudflare-credentials = { file = ../secrets/cloudflare-credentials.age; owner = "acme"; };

  # On-demand Minecraft server
  services.ondemand-minecraft = {
    enable = true;
    memory = "8G";
    environmentFiles = [ config.age.secrets.minecraft-whitelist.path ];
    extraEnvironment = {
      TYPE = "PAPER";
      ENFORCE_WHITELIST = "TRUE";
      VIEW_DISTANCE = "16";
      SIMULATION_DISTANCE = "8";
      NETWORK_COMPRESSION_THRESHOLD = "256";
      ENABLE_RCON = "TRUE";
      JVM_XX_OPTS = builtins.concatStringsSep " " [
        "-XX:+UnlockExperimentalVMOptions"
        "-XX:+AlwaysPreTouch"
        "-XX:+DisableExplicitGC"
        "-XX:+ParallelRefProcEnabled"
        "-XX:+PerfDisableSharedMem"
        "-XX:+UseG1GC"
        "-XX:MaxGCPauseMillis=130"
        "-XX:G1NewSizePercent=28"
        "-XX:G1MaxNewSizePercent=40"
        "-XX:G1HeapRegionSize=8M"
        "-XX:G1ReservePercent=20"
        "-XX:G1MixedGCCountTarget=3"
        "-XX:InitiatingHeapOccupancyPercent=15"
        "-XX:G1MixedGCLiveThresholdPercent=90"
        "-XX:SurvivorRatio=32"
        "-XX:MaxTenuringThreshold=1"
        "-XX:+UseStringDeduplication"
      ];
    };
  };

  security.sudo.wheelNeedsPassword = false;

  # ACME / Let's Encrypt
  security.acme = {
    acceptTerms = true;
    defaults.email = "bensiraphob@gmail.com";
  };

  # Nextcloud
  services.nginx = import ./nginx.nix {};
  services.nextcloud = import ./nextcloud.nix { inherit pkgs; };
  services.anki-sync-server = import ./anki-sync-server.nix {};
  services.postgresql = {
    enable = true;
    ensureDatabases = [ "nextcloud" ];
    ensureUsers = [{ name = "nextcloud"; ensureDBOwnership = true; }];
    settings = {
      shared_buffers = "2GB";
      effective_cache_size = "8GB";
      maintenance_work_mem = "512MB";
      work_mem = "32MB";
      random_page_cost = 1.1;       # SSD-class storage
      effective_io_concurrency = 200;
      wal_buffers = "16MB";
      max_wal_size = "4GB";
      min_wal_size = "1GB";
    };
  };

  # ZFS auto-scrub for silent-corruption detection (trim is already on)
  services.zfs.autoScrub = {
    enable = true;
    interval = "Sat *-*-* 04:00:00";
  };

  # Bump journal retention now that root has headroom (was capped ~460M)
  services.journald.extraConfig = ''
    SystemMaxUse=2G
    MaxRetentionSec=2month
  '';
  systemd.services."nextcloud-setup" = {
    requires = [ "postgresql.service" ];
    after = [ "postgresql.service" ];
  };

  # Disable wait-online (OCI networking is managed outside systemd-networkd)
  systemd.network.wait-online.enable = false;

  # Firewall
  networking.firewall.allowedTCPPorts = [ 80 443 2022 25565 26594 ];
  networking.firewall.allowedUDPPorts = [ ];

  system.stateVersion = "25.11";
}
