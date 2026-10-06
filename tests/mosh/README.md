# Mosh Unicode regressions

The active package is selected through the Home Manager overlay, not the stock
Nixpkgs Mosh package. These checks use that actual package's source and the locked
utf8proc dependency. They do not change the installed package or apply another patch.

Run the native checks (replace the system on Linux):

```sh
nix build .#checks.aarch64-darwin.mosh-unicode .#checks.aarch64-darwin.mosh-graphemes
```

`mosh-unicode` compiles the original implementation extracted from the retained
`home-manager/.config/nixpkgs/mosh/unicode-16.patch` under a different function
name and compares it with the active implementation against explicit expected
widths. Fixtures include U+23F5 (the original macOS disappearing-triangle bug),
combining marks, ZWJ/VS16, CJK, supplementary characters and Unicode 16 emoji.
This is representative regression coverage, not proof of equivalence for all
Unicode codepoints. It prints the linked utf8proc and Unicode data versions.

`mosh-graphemes` builds the selected Mosh package with its existing framebuffer
regression executable enabled. Its 13 cases cover the missing triangle, combining
marks, flags, ZWJ families, VS16 width, cursor motion, line feeds and fallback cells.
A skipped test is a failure here, not a successful check. The normal package is
unchanged; this check derivation enables these tests specifically.

## Why the original patch is retained

The patch fixed a real bug. [Dotfiles PR #39](https://github.com/siraben/dotfiles/pull/39)
repaired its application to Mosh 1.4.0. On April 6, 2026,
[the fork migration](https://github.com/siraben/dotfiles/commit/51a87faf96aa8e52669c43bfd4fe793ceec5e922)
removed the local patch application and `mosh2` wrapper, selecting the Mosh fork
instead. The current lock pins `232a22a2e6eadbb9eb144f84314588190946d721`.
That fork uses utf8proc width data and adds grapheme handling; the old table is
retained here as provenance and a regression oracle, not silently reapplied.

This does not establish that mainline Mosh merged the fix. Upstream
[PR #1289](https://github.com/mobile-shell/mosh/pull/1289) remained open when
checked, with discussion of utf8proc and client/server width consistency.
No live mixed-version client/server session is exercised by these unit checks.
