# Hardware configuration for Beelink Mini S (AZW MINI S, Intel N100)
# Generated from nixos-generate-config, with noatime added for SSD

{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];

  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/16919f63-df70-45ba-8104-c687947db9c5";
    fsType = "ext4";
    options = [ "noatime" "errors=remount-ro" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/EC00-1719";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  swapDevices = [ ];

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  hardware.enableRedistributableFirmware = true;

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
