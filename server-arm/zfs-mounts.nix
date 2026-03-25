{ config, lib, pkgs, ... }:
{
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "deadbeef";

  fileSystems."/nix" = {
    device = "datapool/nix";
    fsType = "zfs";
  };
  fileSystems."/home" = {
    device = "datapool/home";
    fsType = "zfs";
  };
  fileSystems."/var/lib/nextcloud" = {
    device = "datapool/nextcloud";
    fsType = "zfs";
  };
}
