#!/usr/bin/env bash
# PreToolUse hook: block expensive scans and disallowed package installation.
# Reads stdin as a single JSON blob with at least { tool_name, tool_input }.

set -eu

input=$(cat)

tool=$(printf '%s' "$input" | python3 -c 'import json,sys;print(json.load(sys.stdin).get("tool_name",""))' 2>/dev/null || true)

if [ "$tool" != "Bash" ]; then
  exit 0
fi

command=$(printf '%s' "$input" | python3 -c 'import json,sys;print(json.load(sys.stdin).get("tool_input",{}).get("command",""))' 2>/dev/null || true)

# Match a target command as a top-level command, the start of a pipeline,
# inside `$(...)`/backticks, or after `&&`/`||`/`;`. Allow `nix path-info` etc.
prefix='(^|[;&|`]|\$\()[[:space:]]*'

if printf '%s' "$command" | grep -qE "${prefix}find[[:space:]]+/nix/store"; then
  echo "Refusing 'find /nix/store' (filesystem-wide nix store scans are expensive). Use 'nix path-info', 'nix log', or a specific store path instead." >&2
  exit 2
fi

if printf '%s' "$command" | grep -qE "${prefix}rg([[:space:]]|$)"; then
  echo "Refusing 'rg' (ripgrep). Use 'grep -r' / 'grep' instead." >&2
  exit 2
fi

if printf '%s' "$command" | grep -qE "${prefix}(command[[:space:]]+)?([^[:space:];&|]+/)?brew[[:space:]]+install([[:space:]]|$)"; then
  echo "Refusing 'brew install'. Homebrew packages must be installed explicitly by the user." >&2
  exit 2
fi

exit 0
