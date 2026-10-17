#!/usr/bin/env bash
# Managed Claude/Codex hook entrypoint.
set -euo pipefail
exec python3 "$(dirname -- "$0")/block-find-nix-store.py"
