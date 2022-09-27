{ pkgs }:

{
  enable = true;
  package = pkgs.nextcloud24;
  hostName = "cloud.siraben.dev";
  https = true;
  config = {
    overwriteProtocol = "https";
    dbtype = "pgsql";
    dbuser = "nextcloud";
    dbhost = "/run/postgresql"; # nextcloud will add /.s.PGSQL.5432 by itself
    dbname = "nextcloud";
    dbpassFile = "/var/nextcloud-db-pass";
    adminpassFile = "/var/nextcloud-admin-pass";
    adminuser = "root";
  };
}
