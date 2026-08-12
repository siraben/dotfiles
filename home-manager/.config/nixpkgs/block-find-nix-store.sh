#!/usr/bin/env bash
# PreToolUse hook: block expensive Nix store scans and disallowed package installation.
# Python handles both Claude and Codex JSON input shapes; this shell wrapper keeps
# the existing managed hook path stable.

exec python3 -c '
import json
import os
import re
import shlex
import sys


try:
    payload = json.load(sys.stdin)
except (json.JSONDecodeError, OSError, TypeError):
    sys.exit(0)

tool = payload.get("tool_name", "")
if tool not in {"Bash", "shell", "exec_command", "functions.exec"}:
    sys.exit(0)

tool_input = payload.get("tool_input", {})
if isinstance(tool_input, dict):
    command = next(
        (
            value
            for key in ("command", "cmd", "source")
            if isinstance((value := tool_input.get(key)), str)
        ),
        "",
    )
elif isinstance(tool_input, str):
    command = tool_input
else:
    command = ""


def tokens(text):
    try:
        lexer = shlex.shlex(text, posix=True, punctuation_chars=";&|()")
        lexer.whitespace_split = True
        lexer.commenters = ""
        return list(lexer)
    except ValueError:
        # An incomplete quoted command should not make the hook itself fail.
        return re.findall(r"[^\\s;&|()]+|[;&|()]+", text)


def is_command(token):
    token = token.strip("\\\"`{}[],:=")
    token = re.split(r"[:=]", token)[-1]
    return os.path.basename(token) in {"find", "rg"}


def is_store_root(token):
    token = token.strip("\\\"`{}[](),:;")
    return token in {"/nix/store", "/nix/store/"}


def scans_store_root(text, depth=0):
    parsed = tokens(text)
    separators = {";", ";;", "&", "&&", "|", "||", "(", ")"}
    for index, token in enumerate(parsed):
        if not is_command(token):
            continue
        for argument in parsed[index + 1 :]:
            if argument in separators:
                break
            if is_store_root(argument):
                return True

    # Shell -c arguments and code-mode tool calls may contain a command as one
    # quoted token. Recursing preserves direct-command detection in those forms.
    if depth < 3:
        for token in parsed:
            if any(character.isspace() for character in token):
                if scans_store_root(token, depth + 1):
                    return True
    return False


if scans_store_root(command):
    reason = (
        "Refusing find/rg over /nix/store: store-wide scans are expensive. "
        "Use nix path-info, nix log, or a specific known store path instead."
    )
    json.dump(
        {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "deny",
                "permissionDecisionReason": reason,
            }
        },
        sys.stdout,
    )
    sys.stdout.write("\n")
    sys.exit(0)

prefix = r"(^|[;&|`]|\$\()\s*"
shell_wrapper = r"((/[^\s;&|]+/)?(zsh|bash|sh)\s+-[^\s]*c\s+[\"]?)?"
brew_wrapper = r"((command|(/usr/bin/)?env)\s+)?"
action_end = r"([\s\"]|$)"
brew_pattern = (
    prefix
    + shell_wrapper
    + brew_wrapper
    + r"([^\s;&|]+/)?brew\s+"
    + r"((install|reinstall|upgrade)"
    + action_end
    + r"|bundle"
    + action_end
    + r")"
)
if re.search(brew_pattern, command):
    print(
        "Refusing Homebrew package changes. Do not recommend that the user run "
        "them. Add the dependency to an existing project flake.nix; if none "
        "exists, ask how to proceed.",
        file=sys.stderr,
    )
    sys.exit(2)
'
