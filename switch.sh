#!/usr/bin/env bash
set -euo pipefail

# Disable Nix plugins during activation to avoid ABI mismatches between the
# system Nix and the nixpkgs Nix used by Home Manager's activation script.
export NIX_CONFIG="plugin-files ="

if [[ "$(uname -s)" == Linux ]] && [[ -r /etc/profile.d/nix.sh ]]; then
  # Ubuntu's zsh startup files do not source /etc/profile.d automatically.
  # Load the multi-user Nix environment so a fresh installation can bootstrap.
  # shellcheck source=/dev/null
  . /etc/profile.d/nix.sh
fi

if ! command -v home-manager >/dev/null 2>&1; then
  if ! command -v nix >/dev/null 2>&1; then
    echo "::error:: Neither home-manager nor nix is available" >&2
    exit 1
  fi
  exec nix develop --no-write-lock-file --command "$0" "$@"
fi

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
# Pass along any remaining arguments (e.g., --show-trace, -v)
home-manager switch --flake ".#siraben@$FLAKE_HOSTNAME" "$@"
