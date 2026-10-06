"""Test profile selection with fake commands; never activate Home Manager."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / "switch.sh"


class SwitchTest(unittest.TestCase):
    def run_switch(self, system, arch, args=(), bootstrap=False):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            commands = {
                "uname": '#!/bin/bash\ncase "$1" in -s) echo "$TEST_OS";; -m) echo "$TEST_ARCH";; esac\n',
                "home-manager": '#!/bin/bash\nprintf "%s\\n" "$@" > "$TEST_ARGS"\n',
                "nix": '#!/bin/bash\nprintf "%s\\n" "$@" > "$TEST_ARGS"\n',
            }
            for name, contents in commands.items():
                if bootstrap and name == "home-manager":
                    continue
                path = root / name
                path.write_text(contents)
                path.chmod(0o755)
            output = root / "args"
            environment = dict(os.environ, PATH=directory, TEST_OS=system, TEST_ARCH=arch, TEST_ARGS=str(output))
            result = subprocess.run(["/bin/bash", str(SCRIPT), *args], env=environment, text=True, capture_output=True)
            return result, output.read_text().splitlines() if output.exists() else []

    def test_exported_profiles(self):
        for system, arch, profile, expected in [
            ("Linux", "x86_64", None, "x86_64-linux-full"),
            ("Linux", "x86_64", "minimal", "x86_64-linux-minimal"),
            ("Linux", "x86_64", "headless", "x86_64-linux-headless"),
            ("Linux", "aarch64", None, "aarch64-linux-headless"),
            ("Linux", "aarch64", "minimal", "aarch64-linux-minimal"),
            ("Darwin", "arm64", None, "aarch64-darwin-full"),
            ("Darwin", "arm64", "full", "aarch64-darwin-full"),
        ]:
            with self.subTest(system=system, arch=arch, profile=profile):
                arguments = ([profile] if profile else []) + ["--show-trace", "--option", "value with spaces"]
                result, actual = self.run_switch(system, arch, arguments)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(actual, ["switch", "--flake", ".#siraben@" + expected, "--show-trace", "--option", "value with spaces"])

    def test_rejects_unsupported_before_bootstrap(self):
        for system, arch, args in [
            ("Linux", "riscv64", []), ("Darwin", "x86_64", []),
            ("FreeBSD", "x86_64", []), ("Linux", "aarch64", ["full"]),
            ("Darwin", "arm64", ["headless"]), ("Linux", "x86_64", ["typo"]),
        ]:
            with self.subTest(system=system, arch=arch, args=args):
                result, actual = self.run_switch(system, arch, args, bootstrap=True)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("::error::", result.stderr)
                self.assertEqual(actual, [])

    def test_bootstrap_preserves_selected_profile(self):
        result, actual = self.run_switch("Darwin", "arm64", ["full", "--show-trace"], bootstrap=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(actual, ["develop", "--no-write-lock-file", "--command", str(SCRIPT), "full", "--show-trace"])


if __name__ == "__main__":
    unittest.main()
