"""Exercise the extracted writer in an isolated home, never the user's home."""
import json
import os
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
import unittest

WRITER = Path(__file__).resolve().parents[1] / "modules/write-mcp-config.py"


class McpConfigTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.home = Path(self.temporary.name)
        self.target = self.home / ".pi/agent/mcp.json"

    def write(self, declared, import_codex=True):
        declarations = self.home / "declared.json"
        declarations.write_text(json.dumps(declared))
        subprocess.run(
            [sys.executable, str(WRITER), str(declarations),
             str(import_codex).lower()],
            env={**os.environ, "HOME": str(self.home)}, check=True,
        )
        return json.loads(self.target.read_text())["mcpServers"]

    def test_import_conversion_and_declared_precedence(self):
        codex = self.home / ".codex"
        codex.mkdir()
        (codex / "config.toml").write_text('''
[mcp_servers.web]
url = "https://example.invalid/mcp"
http_headers = { "X-Fixed" = "fixture" }
env_http_headers = { "X-Token" = "TEST_TOKEN" }
bearer_token_env_var = "TEST_BEARER"
tool_timeout_sec = 60
enabled = false
[mcp_servers.local]
command = "fixture-command"
args = ["serve"]
env = { "MODE" = "fixture" }
cwd = "/fixture"
[mcp_servers.invalid]
enabled = false
''')
        result = self.write({"web": {"enabled": True, "timeout": 10},
                             "computer-use": {"enabled": False}})
        self.assertEqual(result["web"], {
            "url": "https://example.invalid/mcp", "enabled": True,
            "timeout": 10, "headers": {"X-Fixed": "fixture",
                                      "X-Token": "${TEST_TOKEN}",
                                      "Authorization": "Bearer ${TEST_BEARER}"},
        })
        self.assertEqual(result["local"], {
            "command": "fixture-command", "args": ["serve"],
            "env": {"MODE": "fixture"}, "cwd": "/fixture",
        })
        self.assertNotIn("invalid", result)
        # Preserve the known disabled stub rather than change behavior here.
        self.assertEqual(result["computer-use"], {"enabled": False})

    def test_disabled_import_does_not_read_codex(self):
        codex = self.home / ".codex"
        codex.mkdir()
        (codex / "config.toml").write_text("not valid TOML = [")
        self.assertEqual(self.write({"local": {"command": "fixture"}}, False),
                         {"local": {"command": "fixture"}})

    def test_private_atomic_replacement_and_symlink(self):
        self.target.parent.mkdir(parents=True)
        untouched = self.home / "elsewhere.json"
        untouched.write_text("original")
        self.target.symlink_to(untouched)
        self.write({})
        self.assertFalse(self.target.is_symlink())
        self.assertEqual(untouched.read_text(), "original")
        self.assertEqual(stat.S_IMODE(self.target.stat().st_mode), 0o600)
        self.assertEqual(list(self.target.parent.glob(".mcp.json.*")), [])
        self.assertEqual(self.write({"next": {"command": "fixture"}}),
                         {"next": {"command": "fixture"}})
        self.assertEqual(stat.S_IMODE(self.target.stat().st_mode), 0o600)


if __name__ == "__main__":
    unittest.main()
