{ config, lib, pkgs, ... }:

let
  ircHost = "irc.siraben.dev";
  webHost = "chat.siraben.dev";

  gamjaSrc = pkgs.fetchFromGitHub {
    owner = "siraben";
    repo = "gamja";
    rev = "525697a189b880ed796f56fa3dae594d589c98b8";
    hash = "sha256-QMXK0Oa2DNNZKvXOxGeodGYWo8TIz0XyTSvnTCvEghc=";
  };

  gamja = pkgs.gamja.overrideAttrs (old: {
    version = "siraben-fork-525697a";
    src = gamjaSrc;
    npmDepsHash = "sha256-9MUvDaMIDe9zkPXxcFYGOrHWYEfKqLJofc22w35dQK0=";
    npmDeps = pkgs.fetchNpmDeps {
      src = gamjaSrc;
      hash = "sha256-9MUvDaMIDe9zkPXxcFYGOrHWYEfKqLJofc22w35dQK0=";
    };
  });

  gamjaWeb = gamja.override {
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
    hostName = ircHost;
    listen = [
      "ircs://:6697"
      "ws+insecure://127.0.0.1:8080"
    ];
    tlsCertificate = "/var/lib/acme/${ircHost}/fullchain.pem";
    tlsCertificateKey = "/var/lib/acme/${ircHost}/key.pem";
    httpOrigins = [ "https://${webHost}" ];
    acceptProxyIP = [ "localhost" ];
  };

  # ACME cert dir for an nginx-enableACME host is mode 750 acme:nginx, so
  # soju (DynamicUser=true) joins the nginx group via SupplementaryGroups.
  systemd.services.soju.serviceConfig.SupplementaryGroups = [ "nginx" ];

  services.nginx.virtualHosts.${ircHost} = {
    forceSSL = true;
    enableACME = true;
    locations."/".return = "204";
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

  networking.firewall.allowedTCPPorts = [ 6697 ];
}
