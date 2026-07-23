# server-arm operations

`server-arm` is deployed from this flake. Always name the flake output:

```bash
sudo nixos-rebuild test --flake ~/dotfiles#server-arm
sudo nixos-rebuild switch --flake ~/dotfiles#server-arm
```

Do not run a bare `nixos-rebuild switch` on the host. `/etc/nixos` is not the
source of truth and may be stale.
