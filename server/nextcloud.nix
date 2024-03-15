{ pkgs }:

{
  enable = true;
  package = pkgs.nextcloud28;
  hostName = "cloud.siraben.dev";
  https = true;
  config.adminpassFile = "/var/nextcloud-admin-pass";
}
