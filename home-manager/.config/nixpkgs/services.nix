{ lib, pkgs, isDarwin, isLinux }:

{
  wlsunset = {
    enable = isLinux;
    latitude = "13";
    longitude = "100";
  };
}
