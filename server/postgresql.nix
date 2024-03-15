{}:

{
  enable = true;
  ensureDatabases = [ "nextcloud" ];
  ensureUsers = [
    { name = "nextcloud";
      ensureDBOwnership = true;
    }
  ];
}
