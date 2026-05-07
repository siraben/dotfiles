{ config, lib, pkgs, ... }:

let
  webHost = "chat.siraben.dev";

  gamjaWeb = pkgs.gamja.override {
    gamjaConfig = {
      server = {
        url = "/socket";
        autoconnect = true;
        nick = "$account";
      };
    };
  };
in
{
  services.soju = {
    enable = true;
    hostName = webHost;
    listen = [ "ws+insecure://127.0.0.1:8080" ];
    httpOrigins = [ "https://${webHost}" ];
    acceptProxyIP = [ "localhost" ];
  };

  services.nginx.virtualHosts.${webHost} = {
    forceSSL = true;
    enableACME = true;
    root = "${gamjaWeb}";
    locations."/socket" = {
      proxyPass = "http://127.0.0.1:8080/";
      proxyWebsockets = true;
    };
  };
}
