# Beelink Mini S12 Pro NixOS Configuration

## Installation on Beelink

### 1. Copy configuration to the Beelink

From your main machine with dotfiles repo:
```bash
# Clone your dotfiles on the Beelink
ssh siraben@beelink-ip
git clone https://github.com/siraben/dotfiles.git

# Or copy directly
scp -r ~/dotfiles/beelink siraben@beelink-ip:/tmp/
```

### 2. On the Beelink, prepare the configuration

```bash
# Copy to /etc/nixos
sudo cp -r /tmp/beelink/* /etc/nixos/

# OR if you cloned the repo
sudo cp -r ~/dotfiles/beelink/* /etc/nixos/

# Important: Merge with existing hardware-configuration.nix
# The hardware-configuration.nix from nixos-generate-config has your actual disk UUIDs
sudo mv /etc/nixos/hardware-configuration.nix /etc/nixos/hardware-configuration.nix.bak
sudo nixos-generate-config --root /
# Now merge the generated hardware config with our template
```

### 3. Edit hardware-configuration.nix

Make sure `/etc/nixos/hardware-configuration.nix` has the correct:
- Filesystem UUIDs or labels
- Boot device paths
- Network interface names

### 4. Deploy the configuration

```bash
# First test
sudo nixos-rebuild test --flake /etc/nixos#beelink

# If successful, switch
sudo nixos-rebuild switch --flake /etc/nixos#beelink

# Or boot (apply on next reboot)
sudo nixos-rebuild boot --flake /etc/nixos#beelink
```

## Alternative: Direct deployment from GitHub

Once your dotfiles are pushed:

```bash
# On the Beelink
sudo nixos-rebuild switch --flake github:siraben/dotfiles#beelink
```

## Remote deployment from your main machine

```bash
# From your main machine (requires SSH access)
nixos-rebuild switch --flake .#beelink --target-host siraben@beelink-ip --use-remote-sudo
```

## Adding Beelink to main dotfiles flake

Edit your main `/home/siraben/dotfiles/flake.nix` to include the Beelink configuration:

```nix
{
  description = "Siraben's dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # ... other inputs
  };

  outputs = { self, nixpkgs, home-manager, ... }@allInputs:
    let
      username = "siraben";
    in
    {
      # Your existing home configurations
      homeConfigurations = {
        # ... existing configs
      };
      
      # Add NixOS configurations
      nixosConfigurations = {
        beelink = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./beelink/configuration.nix
            ./beelink/hardware-configuration.nix
            
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.siraben = import ./beelink/home.nix;
            }
          ];
        };
        
        # Add your server config too if desired
        server = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./server/configuration.nix
            ./server/hardware-configuration.nix
          ];
        };
      };
      
      # ... rest of your flake
    };
}
```

## Maintenance Commands

```bash
# Update system
sudo nixos-rebuild switch --upgrade --flake /etc/nixos#beelink

# Garbage collection
sudo nix-collect-garbage -d

# Check system status
systemctl status

# Tailscale login
sudo tailscale up

# Test configuration without switching
sudo nixos-rebuild test --flake /etc/nixos#beelink
```

## Tips

1. Always test before switching
2. Keep a working generation you can rollback to
3. The hardware-configuration.nix must match your actual hardware
4. Use `nixos-generate-config` on the actual machine for accurate hardware detection