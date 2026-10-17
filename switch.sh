#!/usr/bin/env bash
set -euo pipefail

# Disable Nix plugins during activation to avoid ABI mismatches between the
# system Nix and the nixpkgs Nix used by Home Manager's activation script.
export NIX_CONFIG="plugin-files ="

if [[ "$(uname -s)" == Linux ]] && ! command -v nix >/dev/null 2>&1 && [[ -r /etc/profile.d/nix.sh ]]; then
  # Ubuntu's zsh startup files do not source /etc/profile.d automatically.
  # Load the multi-user Nix environment so a fresh installation can bootstrap.
  # shellcheck source=/dev/null
  . /etc/profile.d/nix.sh
fi

# Determine the arch-os-profile triple for the flake
OS_TYPE=$(uname -s)
ARCH=$(uname -m)

case "$OS_TYPE/$ARCH" in
  Linux/x86_64) NIX_ARCH=x86_64; DEFAULT_PROFILE=full ;;
  Linux/aarch64) NIX_ARCH=aarch64; DEFAULT_PROFILE=headless ;;
  Darwin/arm64) NIX_ARCH=aarch64; DEFAULT_PROFILE=full ;;
  *)
    echo "::error:: Unsupported platform: $OS_TYPE/$ARCH" >&2
    exit 1
    ;;
esac

PROFILE=$DEFAULT_PROFILE
case "${1:-}" in
  minimal|headless|full) PROFILE=$1; shift ;;
  ""|-*) ;; # Remaining options are forwarded to Home Manager.
  *)
    echo "::error:: Unknown profile: $1 (expected minimal, headless, or full)" >&2
    exit 1
    ;;
esac

case "$OS_TYPE/$NIX_ARCH/$PROFILE" in
  Linux/x86_64/*|Linux/aarch64/minimal|Linux/aarch64/headless|Darwin/aarch64/full) ;;
  *)
    echo "::error:: Unsupported profile $PROFILE for $OS_TYPE/$ARCH" >&2
    exit 1
    ;;
esac

case "$OS_TYPE" in
  Linux) FLAKE_HOSTNAME="$NIX_ARCH-linux-$PROFILE" ;;
  Darwin) FLAKE_HOSTNAME="$NIX_ARCH-darwin-$PROFILE" ;;
esac

if ! command -v home-manager >/dev/null 2>&1; then
  if ! command -v nix >/dev/null 2>&1; then
    echo "::error:: Neither home-manager nor nix is available" >&2
    exit 1
  fi
  exec nix develop --no-write-lock-file --command "$0" "$PROFILE" "$@"
fi


echo "Switching Home Manager configuration for siraben@$FLAKE_HOSTNAME..."
# Pass along any remaining arguments (e.g., --show-trace, -v)
home-manager switch --flake ".#siraben@$FLAKE_HOSTNAME" "$@"
