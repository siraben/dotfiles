{ pkgs }:

{
  enable = true;
  package = pkgs.nextcloud33;
  hostName = "cloud.siraben.dev";
  database.createLocally = true;
  configureRedis = true;
  maxUploadSize = "1G";
  https = true;
  config.adminpassFile = "/run/agenix/nextcloud-admin-pass";
  config.dbtype = "pgsql";
  ensureUsers = {
    siraben = {
      email = "bensiraphob@gmail.com";
      passwordFile = "/run/agenix/nextcloud-siraben-pass";
    };
  };
  settings.enabledPreviewProviders = [
    "OC\\Preview\\BMP"
    "OC\\Preview\\GIF"
    "OC\\Preview\\JPEG"
    "OC\\Preview\\Krita"
    "OC\\Preview\\MarkDown"
    "OC\\Preview\\MP3"
    "OC\\Preview\\OpenDocument"
    "OC\\Preview\\PNG"
    "OC\\Preview\\TXT"
    "OC\\Preview\\XBitmap"
    "OC\\Preview\\HEIC"
  ];
}
