{ config, lib, pkgs, ... }:

let
  ircHost = "irc.siraben.dev";
  webHost = "chat.siraben.dev";

  gamjaSrc = pkgs.fetchFromGitHub {
    owner = "siraben";
    repo = "gamja";
    rev = "5dd238c933b6eda3777be6b1948c44a1d15d5f30";
    hash = "sha256-gOEGJ5oKdlt33d5Hd4aj5Dp7wb9DzJHQAuIN73xBPKo=";
  };

  gamja = pkgs.gamja.overrideAttrs (old: {
    version = "siraben-fork-5dd238c";
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
