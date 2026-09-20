import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location("package_manifest", Path(__file__).with_name("package-manifest.py"))
manifest = importlib.util.module_from_spec(spec)
spec.loader.exec_module(manifest)


def snapshot(configurations):
    return {"revision": "1234567890abcdef", "configurations": configurations}


def package(name, version):
    return {"name": name, "version": version}


class ManifestTests(unittest.TestCase):
    def test_groups_identical_changes_without_hiding_platform_differences(self):
        before = snapshot({
            "linux": [package("git", "1"), package("git", "1")],
            "arm": [package("git", "1")],
            "mac": [package("git", "2")],
        })
        after = snapshot({name: [package("git", "3")] for name in before["configurations"]})
        self.assertEqual(dict(manifest.changes(before, after)), {
            ("git", ("1",), ("3",)): ["arm", "linux"],
            ("git", ("2",), ("3",)): ["mac"],
        })

    def test_additions_removals_multiple_versions_and_unversioned_packages(self):
        before = snapshot({"linux": [package("old", "1"), package("python", "3.12"), package("python", "3.13")]})
        after = snapshot({"linux": [package("new", ""), package("python", "3.13"), package("python", "3.14")]})
        report = manifest.report(before, after)
        self.assertIn("1 version changes, 1 additions, 1 removals", report)
        self.assertIn("| new | — | (unversioned) |", report)
        self.assertIn("| old | 1 | — |", report)
        self.assertIn("| python | 3.12, 3.13 | 3.13, 3.14 |", report)

    def test_configuration_addition_and_removal(self):
        before = snapshot({"old config": [package("git", "1")]})
        after = snapshot({"new config": [package("git", "1")]})
        self.assertEqual(len(manifest.changes(before, after)), 2)
        report = manifest.report(before, after)
        self.assertIn("old config: 1 → 0", report)
        self.assertIn("new config: 0 → 1", report)

    def test_reordering_and_duplicates_do_not_report_updates(self):
        before = snapshot({"linux": [package("a", "1"), package("b", "2")]})
        after = snapshot({"linux": [package("b", "2"), package("a", "1"), package("a", "1")]})
        self.assertIn("No configured package versions changed.", manifest.report(before, after))

    def test_markdown_cells_are_escaped(self):
        self.assertEqual(manifest.cell("<pkg>|test\nnext"), "&lt;pkg&gt;&#124;test next")

    def test_body_preserves_existing_text_and_is_idempotent(self):
        body = "Lock changes\n\nReviewer notes\n"
        first = manifest.update_body(body, "Report one\n", "https://example.com/run")
        second = manifest.update_body(first + "\nTrailing notes", "Report two\n", "https://example.com/run")
        self.assertIn("Lock changes\n\nReviewer notes", second)
        self.assertIn("Trailing notes", second)
        self.assertNotIn("Report one", second)
        self.assertEqual(second.count(manifest.START), 1)
        self.assertEqual(manifest.update_body(second, "Report two\n", "https://example.com/run"), second)

    def test_malformed_markers_fail(self):
        for body in [manifest.START, manifest.END, manifest.END + manifest.START, manifest.START * 2 + manifest.END]:
            with self.subTest(body=body), self.assertRaises(ValueError):
                manifest.update_body(body, "report", "")

    def test_large_report_links_to_complete_artifact(self):
        report = "### Package changes\n\nSummary\n\n| Package |" + "x" * 70_000
        body = manifest.update_body("Original", report, "https://example.com/run")
        self.assertLess(len(body.encode()), 60_000)
        self.assertIn("Summary", body)
        self.assertIn("too large", body)
        self.assertIn("https://example.com/run", body)
        with self.assertRaises(ValueError):
            manifest.update_body("Original", report, "")


if __name__ == "__main__":
    unittest.main()
