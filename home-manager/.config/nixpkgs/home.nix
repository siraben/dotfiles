{ lib, ... }:
{
  imports = [ ./base.nix ];

  _module.args = {
    profile = lib.mkDefault "full";
    username = lib.mkDefault "siraben";
    timeZone = lib.mkDefault "America/New_York";
  };
}
