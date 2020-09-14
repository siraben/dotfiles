{ config, pkgs, ... }:

let wrapWeb = pkgs.callPackage ./wrapWeb.nix {};
in
{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    loader.grub.enable = true;
    loader.grub.device = "nodev";
    loader.efi.canTouchEfiVariables = true;
    cleanTmpDir = true;
    supportedFilesystems = [ "exfat" "btrfs" ];
  };

  nixpkgs.config.allowUnfree = true;

  networking.networkmanager.enable = true;
  networking.hostName = "siraben-nixos";
  networking.nameservers = [
    "1.0.0.1"
    "1.1.1.1"
  ];
  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  sound.enable = true;
  hardware = {
    bluetooth.enable = true;
    facetimehd.enable = true;
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      extraConfig = "
        load-module module-switch-on-connect
      ";
    };
  };

  services.blueman.enable = true;
  hardware.bluetooth.extraConfig = "
    [General]
    Enable=Source,Sink,Media,Socket
  ";

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  programs.light.enable = true;

  time.timeZone = "Asia/Bangkok";

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;

    fonts = with pkgs; [
      corefonts
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

  environment = {
    systemPackages = with pkgs; [
      (import ./popcorntime.nix)
      arc-theme
      dmenu
      exfat
      gnupg
      killall
      lightlocker
      networkmanager
      nitrogen
      pandoc
      paper-icon-theme
      poppler_utils
      scrot
      system-config-printer
      wpa_supplicant
      xorg.xkill
      xss-lock
    ];
  };
  virtualisation.docker.enable = true;

  programs.zsh.enable = true;
  programs.zsh.promptInit = "";
  programs.ssh.startAgent = true;
  location.provider = "geoclue2";

  services.redshift = {
    enable = true;
    package = pkgs.redshift-wlr;
    temperature.day = 6500;
    temperature.night = 3400;
    brightness.day = "1";
    brightness.night = "1";
  };

  services.logind.extraConfig = ''
      HandlePowerKey=suspend
  '';

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

    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk.extraConfig = ''
         xft-dpi=192
        '';
      };
      defaultSession = "xfce+i3";
    };

    desktopManager = {
      gnome3.enable = true;
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };

    windowManager = {
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
    };
    xkbOptions = "ctrl:nocaps";
    libinput.enable = true;
    layout = "us";

  };

  users = {
    users.siraben = {
      shell = pkgs.zsh;
      isNormalUser = true;
      home = "/home/siraben";
      description = "Ben Siraphob";
      extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" ];
      packages = with pkgs; [
        (wrapWeb "element" "https://app.element.io")
        (wrapWeb "hn" "https://news.ycombinator.com")
        (wrapWeb "neverssl" "http://neverssl.com")
        (wrapWeb "mastodon" "https://mastodon.social")
      ];
    };
  };

  system.stateVersion = "19.09";
}
