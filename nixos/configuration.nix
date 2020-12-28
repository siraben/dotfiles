let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs {
    config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) [
        "broadcom-sta"
        "facetimehd-firmware"
      ];
  };
  lib = pkgs.lib;
in
{
  imports = [ /etc/nixos/hardware-configuration.nix ./rescue_boot.nix ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    cleanTmpDir = true;
    supportedFilesystems = [ "exfat" "btrfs" ];
    kernelParams = [
      "noibrs"
      "noibpb"
      "nopti"
      "nospectre_v2"
      "nospectre_v1"
      "l1tf=off"
      "nospec_store_bypass_disable"
      "no_stf_barrier"
      "mds=off"
      "mitigations=off"
    ];
  };

  # Make unfree software explicit
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "broadcom-sta"
      "facetimehd-firmware"
    ];

  networking.networkmanager.enable = true;
  networking.hostName = "siraben-nixos";
  networking.nameservers = [ "1.0.0.1" "1.1.1.1" ];

  sound.enable = true;
  hardware = {
    bluetooth = {
      enable = true;
      config = {
        General.Enable = lib.concatStringsSep "," [ "Source" "Sink" "Media" "Socket" ];
      };
    };
    facetimehd.enable = true;
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      extraConfig = "load-module module-switch-on-connect";
    };
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
  };
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;
  services.blueman.enable = true;
  services.mbpfan = {
    enable = true;
    lowTemp = 58;
    highTemp = 63;
  };

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  time.timeZone = "Asia/Bangkok";

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;

    fonts = with pkgs; [
      emojione
      noto-fonts
      noto-fonts-cjk
      noto-fonts-extra
      hack-font
      inconsolata
      material-icons
      liberation_ttf
      dejavu_fonts
      terminus_font
      siji
      unifont
    ];
    fontconfig.defaultFonts = {
      monospace = [
        "Hack"
      ];
      sansSerif = [
        "DejaVu Sans"
        "Noto Sans"
      ];
      serif = [
        "DejaVu Serif"
        "Noto Serif"
      ];
    };

  };

  system.autoUpgrade.enable = true;
  system.autoUpgrade.channel = "https://nixos.org/channels/nixos-unstable";

  environment.systemPackages = with pkgs; [ i3status brightnessctl ];

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;

  programs.zsh.enable = true;
  programs.zsh.promptInit = "";
  programs.ssh.startAgent = true;
  location.provider = "geoclue2";

  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako
      xwayland
    ];
  };

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
  '';

  # Disable XHC1 wakeup signal to avoid resume getting triggered some time
  # after suspend. Reboot required for this to take effect.
  services = {
    udev.extraRules = ''SUBSYSTEM=="pci", KERNEL=="0000:00:14.0", ATTR{power/wakeup}="disabled"'';
    tlp.enable = true;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = with pkgs; [
    brlaser
    gutenprint
    gutenprintBin
  ];

  services.gnome3.gnome-keyring.enable = true;
  services.xserver = {
    enable = true;
    displayManager.defaultSession = "sway";
    xkbOptions = "ctrl:nocaps";
    libinput.enable = true;
    layout = "us";
  };

  nix.trustedUsers = [ "root" "siraben" ];

  users = {
    users.siraben = {
      shell = pkgs.zsh;
      isNormalUser = true;
      home = "/home/siraben";
      description = "Ben Siraphob";
      extraGroups = [ "audio" "wheel" "networkmanager" "vboxusers" "docker" ];
    };
  };

  system.stateVersion = "20.09";

}
