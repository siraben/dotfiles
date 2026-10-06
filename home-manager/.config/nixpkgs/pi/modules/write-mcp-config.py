import json
import os
import tempfile
import sys
import tomllib
from pathlib import Path

declared_path = Path(sys.argv[1])
declared = json.loads(declared_path.read_text())
home = Path.home()
servers = {}

codex = home / ".codex" / "config.toml"
if sys.argv[2] == "true" and codex.exists():
    codex_servers = tomllib.loads(codex.read_text()).get("mcp_servers", {})
    for name, entry in codex_servers.items():
        server = {}
        if "url" in entry:
            server["url"] = entry["url"]
            headers = dict(entry.get("http_headers") or {})
            environment_headers = entry.get("env_http_headers") or {}
            for header, variable in environment_headers.items():
                headers[header] = "${" + variable + "}"
            if entry.get("bearer_token_env_var"):
                variable = entry["bearer_token_env_var"]
                headers["Authorization"] = "Bearer ${" + variable + "}"
            if headers:
                server["headers"] = headers
        elif "command" in entry:
            server["command"] = entry["command"]
            for key in ("args", "env", "cwd"):
                if entry.get(key):
                    server[key] = entry[key]
        else:
            continue

        timeout = entry.get("tool_timeout_sec")
        if timeout:
            server["timeout"] = timeout
        if entry.get("enabled") is False:
            server["enabled"] = False
        servers[name] = server

for name, entry in declared.items():
    servers[name] = {**servers.get(name, {}), **entry}

target = home / ".pi" / "agent" / "mcp.json"
target.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
if target.is_symlink():
    target.unlink()

fd, temporary_name = tempfile.mkstemp(
    dir=target.parent,
    prefix=f".{target.name}.",
    text=True,
)
try:
    os.fchmod(fd, 0o600)
    with os.fdopen(fd, "w") as output:
        json.dump({"mcpServers": servers}, output, indent=2)
        output.write("\n")
    os.replace(temporary_name, target)
except BaseException:
    try:
        os.close(fd)
    except OSError:
        pass
    try:
        os.unlink(temporary_name)
    except FileNotFoundError:
        pass
    raise
