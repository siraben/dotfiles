{}:

{
  enable = true;
  ensureDatabases = [ "nextcloud" ];
  ensureUsers = [
    { name = "nextcloud";
      ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
    }
  ];
}
