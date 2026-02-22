let
  siraben = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILA4yYM7dggFdE6jy4/+LJf3icYiT6PxSboOHo+/zYad bensiraphob@gmail.com";
  server-arm = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA4ErcFxDbhegjlnER/Y7lW+O5ZDzy+t7nlBw3IaK3B7 root@nixos-oci";
in
{
  "secrets/minecraft-whitelist.age".publicKeys = [ siraben server-arm ];
}
