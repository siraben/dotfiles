{}:

{
  enable = true;

  # Use recommended settings
  recommendedGzipSettings = true;
  recommendedOptimisation = true;
  recommendedProxySettings = true;
  recommendedTlsSettings = true;

  # Only allow PFS-enabled ciphers with AES256
  sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";

  # Setup Nextcloud virtual host to listen on ports
  virtualHosts = {
    "cloud.siraben.dev" = {
      forceSSL = true;
      enableACME = true;
    };
    "siraben.dev" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:4000";
        extraConfig = "proxy_ssl_server_name on; proxy_pass_header Authorization;";
      };
    };
  };
}
