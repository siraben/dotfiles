#!/usr/bin/env bash
# PreToolUse hook: block any Bash command that runs `find /nix/store`.
# Reads stdin as a single JSON blob with at least { tool_name, tool_input }.

set -eu

input=$(cat)

tool=$(printf '%s' "$input" | python3 -c 'import json,sys;print(json.load(sys.stdin).get("tool_name",""))' 2>/dev/null || true)

if [ "$tool" != "Bash" ]; then
  exit 0
fi

command=$(printf '%s' "$input" | python3 -c 'import json,sys;print(json.load(sys.stdin).get("tool_input",{}).get("command",""))' 2>/dev/null || true)

# Match `find /nix/store` as a top-level command, the start of a pipeline,
# inside `$(...)`/backticks, or after `&&`/`||`/`;`. Allow `nix path-info` etc.
if printf '%s' "$command" | grep -qE '(^|[;&|`]|\$\()[[:space:]]*find[[:space:]]+/nix/store'; then
  printf '%s\n' '{"decision":"block","reason":"Refusing `find /nix/store` (filesystem-wide nix store scans are expensive). Use `nix path-info`, `nix log`, or a specific store path instead."}'
  exit 0
fi

exit 0
