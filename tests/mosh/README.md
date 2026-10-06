# Mosh Unicode regressions

The active package is selected through the Home Manager overlay, not the stock
Nixpkgs Mosh package. These checks use that actual package's source and the locked
utf8proc dependency. They do not change the installed package or apply another patch.

Run the native checks (replace the system on Linux):

```sh
nix build .#checks.aarch64-darwin.mosh-unicode .#checks.aarch64-darwin.mosh-graphemes
```

`mosh-unicode` compiles the active implementation against explicit expected
widths. Fixtures include U+23F5 (the original macOS disappearing-triangle bug),
combining marks, ZWJ/VS16, CJK, supplementary characters and Unicode 16 emoji.
During the initial audit, the historical patch was also compiled under a different
function name and agreed on all 12 fixtures on macOS and Linux. The permanent
check has no dependency on the unused patch artifact and remains valid after #89.
This is representative regression coverage, not proof of equivalence for all
Unicode codepoints. It prints the linked utf8proc and Unicode data versions.

`mosh-graphemes` builds the selected Mosh package with its existing framebuffer
regression executable enabled. Its 13 cases cover the missing triangle, combining
marks, flags, ZWJ families, VS16 width, cursor motion, line feeds and fallback cells.
A skipped test is a failure here, not a successful check. The normal package is
unchanged; this check derivation enables these tests specifically.

## Where the original fix went

The patch fixed a real bug. [Dotfiles PR #39](https://github.com/siraben/dotfiles/pull/39)
repaired its application to Mosh 1.4.0. On April 6, 2026,
[the fork migration](https://github.com/siraben/dotfiles/commit/51a87faf96aa8e52669c43bfd4fe793ceec5e922)
removed the local patch application and `mosh2` wrapper, selecting the Mosh fork
instead. That first fork pin,
[`cf7a85f`](https://github.com/siraben/mosh/commit/cf7a85f84971a62b5cc05d6c5aca750b7f4a5465),
already implemented utf8proc widths and grapheme support. The current lock pins `232a22a2e6eadbb9eb144f84314588190946d721`.
That fork uses utf8proc width data and adds grapheme handling; the old patch is
not reapplied. Its artifact can be removed without changing the active package;
its historical implementation remains available in Git.

This does not establish that mainline Mosh merged the fix. Upstream
[PR #1289](https://github.com/mobile-shell/mosh/pull/1289) remained open when
checked, with discussion of utf8proc and client/server width consistency.
No live mixed-version client/server session is exercised by these unit checks.
