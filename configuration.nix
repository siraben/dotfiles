# configuration.nix
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    cleanTmpDir = true;
  };
  nixpkgs.config.allowUnfree = true;

  networking.networkmanager.enable = true;
  networking.hostName = "nixos"; # Define your hostname.
  swapDevices = [ { device = "/var/swap"; size = 4096; } ];
  hardware.bluetooth.enable = true;
  hardware.facetimehd.enable = true;

  powerManagement.enable = true;
  programs.light.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Bangkok";

  fonts.enableFontDir = true;
  fonts.enableCoreFonts = true;
  fonts.enableGhostscriptFonts = true;

  fonts.fonts = with pkgs; [
    hack-font
    inconsolata
    liberation_ttf
    dejavu_fonts
    gentium
    terminus_font
  ];

  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "emacs";
    };
    systemPackages = with pkgs; [
      anki
      binutils
      chromium
      emacs
      firefox
      gcc
      gimp
      git
      gnumake
      guile
      htop
      i3-gaps
      keepassxc
      killall
      libreoffice
      mu
      networkmanager
      networkmanagerapplet
      nextcloud-client
      nitrogen
      ranger
      redshift
      riot-web
      stow
      terminator
      texlive.combined.scheme-full
      thunderbird
      unclutter
      vim
      vlc
      wget
      wpa_supplicant
      zile
      zsh
    ];
  };
  # nextcloud-client = pkgs.nextcloud-client.override { withGnomeKeyring = true; libgnome-keyring = pkgs.gnome3.libgnome-keyring; };

  systemd.user.services."unclutter" = {
    enable = true;
    description = "hide cursor after X seconds idle";
    wantedBy = [ "default.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.unclutter}/bin/unclutter";
  };

  services.redshift = {
     enable = true;
     latitude = "13";
     longitude = "100";
     provider = "manual";
     temperature.day = 6500;
     temperature.night = 3500;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  services.xserver = {
    enable = true;
    # desktopManager.plasma5.enable = true;
    displayManager.sddm.enable = true;
    xkbOptions = "ctrl:nocaps";
    libinput.enable = true;
    layout = "us";

    desktopManager = {
      default = "xfce";
      xterm.enable = false;
      xfce = {
        enable = true;
        noDesktop = true;
        enableXfwm = false;
      };
    };
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };
  systemd.user.services.nm-applet = {
    description = "Network Manager applet";
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];
    path = [ pkgs.dbus ];
    serviceConfig = {
      ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
      RestartSec = 3;
      Restart = "always";
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.siraben = {
    isNormalUser = true;
    home = "/home/siraben";
    description = "Ben Siraphob";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
