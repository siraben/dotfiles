{}:

{
  enable = true;
  fqdn = "mail.siraben.dev";
  domains = [ "siraben.dev" ];
  loginAccounts = {
    "siraben@siraben.dev" = {
      hashedPasswordFile = "/var/mailserver-pass";
    };
  };
  certificateScheme = 3;
}
