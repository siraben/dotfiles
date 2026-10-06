#!/usr/bin/env bash
# Run from the repository root, with nixfmt, actionlint and shellcheck on PATH.
# Formatting is enforced on changes; existing formatting debt stays out of
# unrelated reviews. Pass the PR base commit (or the previous push commit).
set -euo pipefail
base=${1:?Usage: scripts/check-quality.sh BASE_REVISION}

actionlint
while IFS= read -r -d '' script; do
  shellcheck "$script"
done < <(git ls-files -z -- '*.sh')

# A new branch push can have an all-zero predecessor. Check every Nix file then.
if [[ "$base" =~ ^0+$ ]]; then
  files=(git ls-files -z -- '*.nix')
else
  git rev-parse --verify "$base^{commit}" >/dev/null
  files=(git diff --name-only --diff-filter=ACMR -z "$base" HEAD -- '*.nix')
fi
while IFS= read -r -d '' source; do
  nixfmt --check "$source"
done < <("${files[@]}")
