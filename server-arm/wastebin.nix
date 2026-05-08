{ config, lib, pkgs, ... }:

let
  host = "paste.siraben.dev";
in
{
  services.wastebin = {
    enable = true;
    settings = {
      WASTEBIN_ADDRESS_PORT = "127.0.0.1:8088";
      WASTEBIN_BASE_URL = "https://${host}";
      WASTEBIN_TITLE = "siraben paste";
      WASTEBIN_MAX_BODY_SIZE = 10485760;
    };
  };

  services.nginx.virtualHosts.${host} = {
    forceSSL = true;
    enableACME = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:8088";
      extraConfig = ''
        client_max_body_size 10m;
        # Anyone can read pastes (GET/HEAD/OPTIONS); only authed users can
        # create/modify/delete. htpasswd file is managed out-of-band at
        # /var/lib/wastebin-htpasswd (root:nginx 640).
        limit_except GET HEAD OPTIONS {
          auth_basic "siraben paste";
          auth_basic_user_file /var/lib/wastebin-htpasswd;
        }
      '';
    };
  };
}
