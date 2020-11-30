let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  lib = pkgs.lib;
in
{
  imports = [ /etc/nixos/hardware-configuration.nix ./rescue_boot.nix ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    cleanTmpDir = true;
    supportedFilesystems = [ "exfat" "btrfs" ];
  };
  nixpkgs.config.allowUnfree = true;

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
  };
  services.blueman.enable = true;

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

  nix.trustedUsers = [ "root" "siraben" ];

  users = {
    users.siraben = {
      shell = pkgs.zsh;
      isNormalUser = true;
      home = "/home/siraben";
      description = "Ben Siraphob";
      extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" ];
    };
  };

  system.stateVersion = "20.09";
}
