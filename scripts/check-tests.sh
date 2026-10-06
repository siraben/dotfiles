#!/usr/bin/env bash
# Run from the repository root with Node 24 and Python 3 on PATH.
set -euo pipefail
shopt -s nullglob
node --experimental-vm-modules --experimental-strip-types --test \
  home-manager/.config/nixpkgs/test-pi-codex-usage.mjs tests/*.test.ts
# Focused sibling PRs can add policy/architecture tests without making the
# deletion-only base depend on files that do not exist there yet.
if [[ -d tests ]]; then
  python3 -m unittest discover -s tests -p 'test_*.py'
fi
