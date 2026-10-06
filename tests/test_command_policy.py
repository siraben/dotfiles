"""Exercise the hook protocol against the same cases as the Pi extension."""
import json
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[1]
HOOK = ROOT / "home-manager/.config/nixpkgs/block-find-nix-store.sh"
FIXTURES = json.loads((ROOT / "tests/command-policy.json").read_text())


class CommandPolicyTest(unittest.TestCase):
    def test_shared_commands(self):
        for fixture in FIXTURES:
            for tool, field in [("Bash", "command"), ("shell", "command"), ("exec_command", "cmd"), ("functions.exec", "source")]:
                with self.subTest(case=fixture["name"], tool=tool):
                    result = subprocess.run(["bash", str(HOOK)], input=json.dumps({"tool_name": tool, "tool_input": {field: fixture["command"]}}), text=True, capture_output=True)
                    expected = fixture["hook"]
                    self.assertEqual(result.returncode, 2 if expected == "brew" else 0)
                    if expected == "scan":
                        self.assertEqual(json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"], "deny")
                    else:
                        self.assertEqual(result.stdout, "")
                    if expected == "brew":
                        self.assertIn("Homebrew", result.stderr)
                    else:
                        self.assertEqual(result.stderr, "")

    def test_ignored_payloads(self):
        for payload in ["not json", "{}", '{"tool_name":"Read","tool_input":{"command":"find /"}}']:
            result = subprocess.run(["bash", str(HOOK)], input=payload, text=True, capture_output=True)
            self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "", ""))


if __name__ == "__main__":
    unittest.main()
