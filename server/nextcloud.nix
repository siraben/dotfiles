{ pkgs }:

{
  enable = true;
  package = pkgs.nextcloud31;
  hostName = "cloud.siraben.dev";
  database.createLocally = true;
  configureRedis = true;
  maxUploadSize = "1G";
  https = true;
  config.adminpassFile = "/var/nextcloud-admin-pass";
  config.dbtype = "sqlite";
  ensureUsers = {
    siraben = {
      email = "bensiraphob@gmail.com";
      passwordFile = "/var/nextcloud-siraben-pass";
    };
  };
  extraOptions.enabledPreviewProviders = [
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
