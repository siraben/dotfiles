{ lib, pkgs }:

{
  ssh-agent = {
    enable = true;
  };

  wlsunset = {
    enable = false;
    latitude = "36";
    longitude = "-86";
  };
}
