#!/usr/bin/env bash
set -euo pipefail

# Determine the arch-os-profile triple for the flake
OS_TYPE=$(uname -s)
ARCH=$(uname -m)

case "$OS_TYPE" in
  Linux)
    NIX_ARCH=$([[ "$ARCH" == "aarch64" ]] && echo "aarch64" || echo "x86_64")
    PROFILE="${1:-}"
    case "$PROFILE" in
      minimal|headless|full)
        shift || true
        ;;
      *)
        # Default: aarch64 -> headless, x86_64 -> full
        if [[ "$NIX_ARCH" == "aarch64" ]]; then
          PROFILE="headless"
        else
          PROFILE="full"
        fi
        ;;
    esac
    FLAKE_HOSTNAME="${NIX_ARCH}-linux-${PROFILE}"
    ;;
  Darwin)
    NIX_ARCH=$([[ "$ARCH" == "arm64" ]] && echo "aarch64" || echo "x86_64")
    PROFILE="full"
    FLAKE_HOSTNAME="${NIX_ARCH}-darwin-${PROFILE}"
    ;;
  *)
    echo "::error:: Unsupported OS type: $OS_TYPE" >&2
    exit 1
    ;;
esac

echo "Switching Home Manager configuration for siraben@$FLAKE_HOSTNAME..."
# Disable nix plugins during activation to avoid ABI mismatches between the
# system nix and the nixpkgs nix used by home-manager's activate script.
export NIX_CONFIG="plugin-files ="
# Pass along any remaining arguments (e.g., --show-trace, -v)
home-manager switch --flake ".#siraben@$FLAKE_HOSTNAME" "$@"
