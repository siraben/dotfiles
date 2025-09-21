{
  config,
  pkgs,
  lib,
  ...
}:

let
  sshKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDGSUM4kaMhd0SaR7qXXbdUtTRy4uC9cPqbpfJ3QP+zZZfeU/rMg4Gv8w10JmFfvrPWFCRgZ3su7ewN+We3rbmN2qDxArOPdzjBfQ/N33epz1Th3fpswdoLmyYtUxeugqGo9TM2e4K4OwJwJnrOvfbfqqhkwvCYgcHzjsA2I1tEThI6eKcYInhq8IOSmwtnGNvl77HrH6cnXcrX3OK9XVSeHVzJKVwzb0IDsdr2fUdwCQZnlfeVj/LuXlDn5hueLQbi5qzyMdI+KeQA3i2iu+aq35Yn7ubOZjQ0kM0uCcm6nhWp4bXtSFGA4Kj4GwOvpTVULSdIW7mu6f37/OTW9MyuJnsFYxJgDUMB8giH4LHEOI9ZhYpkrvO01Lh2igCCVe8GGqDkpu9OQEzWRnFdE3oFH9QbPSWtniX2ZWH/zkoxP2iVGxJkcOiOZGAEsF19skyaCDyu0ZwC8xGzu8S6ZZic+BHeeXXstiquMuTemlU8dqxtmo+cw2xo7JqSZu20EPKjlXz/V6cVTfPQXeH+ANRz4bihdTfHEIEmAXH9PU4vni63loJvSdGqTITUtmQDpeSu+e5qF48IX+Hu+x+Hr/1HhGVn1o2G1DjutM+9BobHiMAq+rh/tPMc1zGlsdCyXf3121WBOrFG4fD/ZCdJoMAZzKaqmaIrcLvQbEVCrRHXNw== bensiraphob@gmail.com"
  ];
in
{
  imports = [ ];

  ##############################################################################
  # Boot & Kernel
  ##############################################################################
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    tmp.cleanOnBoot = true;

    kernel.sysctl = {
      "kernel.sched_autogroup_enabled" = 1;
      "fs.inotify.max_user_watches" = 1048576;
      "fs.inotify.max_user_instances" = 1024;
      "vm.swappiness" = 10;
    };

    # Newer kernel for HW support
    kernelPackages = pkgs.linuxPackages_latest;

    # Intel N100 graphics tuning
    kernelParams = [
      "i915.enable_guc=2"
      "i915.enable_fbc=1"
    ];

    # Ensure HDMI audio path shows up
    kernelModules = [
      "snd_hda_intel"
      "snd_hda_codec_hdmi"
    ];
    extraModprobeConfig = ''
      options snd-intel-dspcfg dsp_driver=1
    '';
  };

  ##############################################################################
  # Locale, Console, Time
  ##############################################################################
  time.timeZone = "Asia/Bangkok";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
  };
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  ##############################################################################
  # Networking & Name Resolution
  ##############################################################################
  networking = {
    hostName = "beelink";
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
      dns = "systemd-resolved";
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
    };
  };
  services.resolved = {
    enable = true;
    dnssec = "allow-downgrade";
    fallbackDns = [
      "1.1.1.1"
      "9.9.9.9"
      "8.8.8.8"
      "2606:4700:4700::1111"
      "2620:fe::fe"
      "2001:4860:4860::8888"
    ];
  };

  hardware.enableRedistributableFirmware = true;

  zramSwap = {
    enable = true;
    memoryPercent = 50;
  };
  systemd.oomd.enable = true;

  ##############################################################################
  # Desktop (Plasma 6 + SDDM Wayland)
  ##############################################################################
  services = {
    xserver.enable = true; # Xwayland apps work fine
    desktopManager.plasma6.enable = true;
    displayManager = {
      sddm.enable = true;
      sddm.wayland.enable = true;
      autoLogin = {
        enable = true;
        user = "siraben";
      };
    };

    # Printing
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint ];
    };

    # Avahi/mDNS
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };

    # Firmware updates
    fwupd.enable = true;

    # Power/Thermals
    thermald.enable = true;
    power-profiles-daemon.enable = false; # we use TLP instead
    tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 100;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 60;
      };
    };

    journald.extraConfig = ''
      SystemMaxUse=1G
      MaxRetentionSec=14day
    '';

    # SSD trim
    fstrim.enable = true;

    # Tailscale
    tailscale.enable = true;

    # OpenSSH (keys only)
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "no";
        KbdInteractiveAuthentication = false;
      };
    };
  };

  ##############################################################################
  # Audio (PipeWire + WirePlumber, prefer HDMI/DP by default)
  ##############################################################################
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;

    wireplumber = {
      enable = true;
      # Append a rule to prefer HDMI/DP sinks as default without overwriting others
      configPackages = [
        (pkgs.writeTextDir "share/wireplumber/main.lua.d/50-default-hdmi.lua" ''
          -- Prefer any HDMI/DisplayPort sink as the default output
          rule = {
            matches = { { { "node.name", "matches", "alsa_output.*hdmi.*" }, }, },
            apply_properties = { ["node.default"] = true, ["priority.session"] = 200 },
          }
          table.insert(alsa_monitor.rules, rule)
        '')
      ];
    };
  };

  ##############################################################################
  # Graphics (Intel N100 / VAAPI)
  ##############################################################################
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      intel-media-driver
      intel-compute-runtime
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  environment.variables = {
    LIBVA_DRIVER_NAME = "iHD";
    VDPAU_DRIVER = "va_gl";
  };


  ##############################################################################
  # Bluetooth
  ##############################################################################
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };

  ##############################################################################
  # User
  ##############################################################################
  users.users.siraben = {
    isNormalUser = true;
    description = "Ben Siraphob";
    shell = pkgs.zsh;
    extraGroups = [
      "wheel"
      "networkmanager"
      "audio"
      "video"
      "docker"
    ];
    openssh.authorizedKeys.keys = sshKeys;
  };

  ##############################################################################
  # Packages & Programs
  ##############################################################################
  environment.systemPackages = with pkgs; [
    # Terminal / CLI
    vim
    neovim
    git
    wget
    curl
    htop
    btop
    neofetch
    tree
    ripgrep
    fd
    bat
    eza
    zoxide
    tmux
    pciutils
    usbutils
    lshw
    inxi
    iotop
    iftop
    nethogs
    nmap
    dig
    traceroute
    mtr
    wireguard-tools

    # Dev
    gcc
    gnumake
    python3
    nodejs
    rustup
    go

    # KDE / Plasma tools
    kdePackages.kate
    kdePackages.konsole
    kdePackages.dolphin
    kdePackages.ark
    kdePackages.spectacle
    kdePackages.kcalc
    kdePackages.plasma-systemmonitor
    kdePackages.partitionmanager
    kdePackages.filelight

    # Apps & media
    firefox
    unzip
    p7zip
    rar
    ffmpeg-full

    # Audio helpers
    alsa-utils
    pavucontrol
  ];

  programs = {
    zsh.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    dconf.enable = true;
  };

  ##############################################################################
  # Virtualization
  ##############################################################################
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
  };

  ##############################################################################
  # Fonts
  ##############################################################################
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    jetbrains-mono
    font-awesome
  ];

  ##############################################################################
  # Nix & Security
  ##############################################################################
  nixpkgs.config.allowUnfree = true;

  nix = {
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users = [
        "root"
        "siraben"
      ];
      auto-optimise-store = true;

      # Helpful when using remote builders/targets:
      builders-use-substitutes = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  security = {
    sudo.wheelNeedsPassword = false;
  };

  ##############################################################################
  # System
  ##############################################################################
  systemd.targets.sleep.enable = false;
  systemd.targets.suspend.enable = false;
  systemd.targets.hibernate.enable = false;
  systemd.targets.hybrid-sleep.enable = false;

  system.stateVersion = "25.05";
}
