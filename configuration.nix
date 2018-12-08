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
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  nixpkgs.config.allowUnfree = true;
  boot.cleanTmpDir = true;
  networking.hostName = "nixos"; # Define your hostname.

  hardware.bluetooth.enable = true;
  hardware.facetimehd.enable = true;

  powerManagement.enable = true;
  programs.light.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Bangkok";

  environment.systemPackages = with pkgs; [
    wget vim emacs git zile stow zsh firefox redshift
    hack-font thunderbird mu keepassxc nextcloud-client
  ];

  services.redshift = {
     latitude = 13;
     longitude = 100;
     provider = "manual";
     temperature.day = 6500;
     temperature.night = 3500;
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  services.openssh.enable = true;
  networking.firewall.enable = true;

  services.printing.enable = true;


  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "ctrl:swapcaps";

  services.xserver.libinput.enable = true;

  # services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.displayManager.gdm.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.siraben = {
    isNormalUser = true;
    home = "/home/siraben";
    description = "Ben Siraphob";
    extraGroups = [ "wheel" "networkmanager" ];    
  };


  system.stateVersion = "18.09"; # Did you read the comment?

}
