{ config, pkgs, ... }:

let wrapWeb = pkgs.callPackage ./wrapWeb.nix {}; in 
{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    loader.systemd-boot.enable = true;
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
  # swapDevices = [ { device = "/var/swap"; size = 8192; } ];

  sound.enable = true;
  hardware = {
    bluetooth.enable = true;
    facetimehd.enable = true;
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
  };

  powerManagement.enable = true;
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

  environment = {
    variables = {
      EDITOR = pkgs.lib.mkOverride 0 "emacsclient";
    };
    systemPackages = with pkgs; [
      (import ./popcorntime.nix)
      afl
      anki
      arc-theme
      asciinema
      aspell
      aspellDicts.en
      atool
      binutils
      borgbackup
      brave
      cabal-install
      coq_8_8
      djvu2pdf
      dragon-drop
      emacs
      evince
      exfat
      feh
      ffmpeg
      firefox
      font-awesome-ttf
      gcc
      gdb
      geekbench
      gforth
      ghc
      gimp
      git
      gnome3.cheese
      gnumake
      gnupg
      gnuplot
      gparted
      guile
      highlight
      htop
      i3-gaps
      imagemagick7
      keepassxc
      killall
      kitty
      ledger
      libreoffice
      lightlocker
      man-pages
      mawk
      mdk
      mediainfo
      mpd
      mpv
      msmtp
      mu
      multimc
      musescore
      networkmanager
      nextcloud-client
      nitrogen
      offlineimap
      okular
      pandoc
      paper-icon-theme
      poppler_utils
      python3
      python37Packages.pygments
      python37Packages.virtualenv
      ranger
      redshift
      rhythmbox
      rofi
      sbcl
      scrot
      silver-searcher
      smlnj
      stow
      spotify
      system-config-printer
      tdesktop
      texlive.combined.scheme-small
      the-powder-toy
      thunderbird
      tldr
      tmux
      tor-browser-bundle-bin
      transmission-gtk
      tree
      unzip
      vim
      vlc
      w3m
      wget
      whois
      wpa_supplicant
      xorg.xkill
      xss-lock
      youtube-dl
      zathura
      zile
      zip
      zsh
    ];
  };
  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.guest.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  virtualisation.virtualbox.host.enableHardening = true;

  # nextcloud-client = pkgs.nextcloud-client.override { withGnomeKeyring = true; libgnome-keyring = pkgs.gnome3.libgnome-keyring; };

  programs.zsh.enable = true;
  programs.zsh.promptInit = ""; # Clear this to avoid a conflict with oh-my-zsh
  programs.ssh.startAgent = true;
  location.provider = "geoclue2";

  services.redshift = {
    enable = true;
    temperature.day = 6500;
    temperature.night = 3400;
    brightness.day = "1";
    brightness.night = "1";
  };

  services.logind.extraConfig = ''
      HandlePowerKey=suspend
  '';

  nixpkgs.config.packageOverrides = pkgs: {
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
    
    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk.extraConfig = ''
         xft-dpi=192
        '';
      };
      defaultSession = "xfce+i3";
    };
    xkbOptions = "ctrl:nocaps";
    libinput.enable = true;
    layout = "us";
    
    desktopManager = {
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
  };

  users = {
    users.siraben = {
      shell = pkgs.zsh;
      isNormalUser = true;
      home = "/home/siraben";
      description = "Ben Siraphob";
      extraGroups = [ "wheel" "networkmanager" "vboxusers" "docker" ];
      packages = with pkgs; [
        (wrapWeb "riot" "https://riot.im/app")
        (wrapWeb "hn" "https://news.ycombinator.com")
        (wrapWeb "neverssl" "http://neverssl.com")
        (wrapWeb "mastodon" "https://mastodon.social")
      ];
    };
  };

  system.stateVersion = "19.09";
}
