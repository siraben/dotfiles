#!/usr/bin/env bash
# Keep the managed Claude/Codex hook entrypoint stable.
set -euo pipefail
exec python3 "$(dirname -- "$0")/block-find-nix-store.py"
