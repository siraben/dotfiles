# configuration.nix
# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./sddm.nix
      ./tilem.nix
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

  sound.enable = true;
  hardware = {
    bluetooth.enable = true;
    facetimehd.enable = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
  };

  powerManagement.enable = true;
  programs.light.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Bangkok";

  fonts = {
    enableFontDir = true;
    enableCoreFonts = true;
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
    fontconfig.ultimate.enable = true;
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

  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "emacs";
    };
    systemPackages = with pkgs; [
      anki
      arc-theme
      aspell
      aspellDicts.en
      binutils
      borgbackup
      # brave
      emacs
      evince
      exfat
      ffmpeg
      firefox
      font-awesome-ttf
      gcc
      gforth
      gimp
      git
      gnumake
      gparted
      gpicview
      guile
      htop
      i3-gaps
      imagemagick7
      inkscape
      keepassxc
      killall
      libreoffice
      lightlocker
      # magic-wormhole
      mpd
      mpv
      msmtp
      mu
      networkmanager
      nextcloud-client
      nitrogen
      offlineimap
      okular
      pandoc
      paper-icon-theme
      polybar
      python3
      qutebrowser
      rambox
      # highlight
      atool
      lynx
      mediainfo
      ranger
      redshift
      rhythmbox
      riot-web
      rofi
      rxvt_unicode
      scrot
      silver-searcher
      stow
      system-config-printer
      terminator
      texlive.combined.scheme-full
      thunderbird
      tmux
      tor-browser-bundle-bin
      transmission-gtk
      tree
      unzip
      vim
      vlc
      wget
      wpa_supplicant
      xss-lock
      youtube-dl
      zile
      zip
    ];
  };
  # nextcloud-client = pkgs.nextcloud-client.override { withGnomeKeyring = true; libgnome-keyring = pkgs.gnome3.libgnome-keyring; };

  services.redshift = {
    enable = true;
    latitude = "13";
    longitude = "100";
    provider = "manual";
    temperature.day = 6500;
    temperature.night = 3000;
    brightness.day = "1";
    brightness.night = "1";
  };

  nixpkgs.config.packageOverrides = pkgs: {
    polybar = pkgs.polybar.override {
      alsaSupport = true;
      i3GapsSupport = true;
      # githubSupport = true;
      mpdSupport = true;
    };
    emacs = pkgs.emacs.override {
     imagemagick = pkgs.imagemagick;
    };
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
    # desktopManager.plasma5.enable = true;
    # displayManager.sddm = {
    #   enable = true;
    #   enableHidpi = true;
    #   theme = "sugar-light";
    #   extraConfig = ''
    #     ForceHideCompletePassword=true
    #   '';
    #  };
    
    displayManager.lightdm.enable = true;
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
    windowManager = {
      default = "i3";
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
    };
  };

  # services.xserver.desktopManager.gnome3.enable = true;
  # services.xserver.displayManager.gdm.enable = true;


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.siraben = {
    shell = pkgs.bash;
    isNormalUser = true;
    home = "/home/siraben";
    description = "Ben Siraphob";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # mine.workstation.recap.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}

