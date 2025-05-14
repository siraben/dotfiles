#!/usr/bin/env bash
set -euo pipefail

# Determine hostname for the flake
OS_TYPE=$(uname -s)
ARCH=$(uname -m)
FLAKE_HOSTNAME=""

if [[ "$OS_TYPE" == "Linux" ]]; then
  # Default to 'linux', user can specify 'minimal' as an argument
  if [[ "${1:-}" == "minimal" ]]; then # Check first argument safely
    FLAKE_HOSTNAME="minimal"
    shift || true # Consume the 'minimal' argument if present
  else
    FLAKE_HOSTNAME="linux" # This is x86_64-linux by flake definition
  fi
elif [[ "$OS_TYPE" == "Darwin" ]]; then
  if [[ "$ARCH" == "x86_64" ]]; then
    FLAKE_HOSTNAME="macos-x86_64"
  elif [[ "$ARCH" == "arm64" ]]; then # uname -m on Apple Silicon is arm64
    FLAKE_HOSTNAME="macos-aarch64" # Flake uses aarch64
  else
    echo "::error:: Unsupported macOS architecture: $ARCH" >&2
    exit 1
  fi
else
  echo "::error:: Unsupported OS type: $OS_TYPE" >&2
  exit 1
fi

echo "Switching Home Manager configuration for siraben@$FLAKE_HOSTNAME..."
# Pass along any remaining arguments (e.g., --show-trace, -v)
home-manager switch --flake ".#siraben@$FLAKE_HOSTNAME" "$@"
