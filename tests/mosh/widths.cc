#include <cstdio>
#include <cwchar>
#include <utf8proc.h>
#include "src/terminal/moshwcwidth.h"
int legacy_mosh_wcwidth(wchar_t);
struct Fixture { wchar_t cp; int width; const char *name; };
int main() {
  const Fixture fixtures[] = {
    {0x23f5, 1, "original macOS missing triangle"},
    {0x41, 1, "ASCII"}, {0xe9, 1, "Latin"},
    {0x301, 0, "combining acute"}, {0x200d, 0, "zero width joiner"},
    {0xfe0f, 0, "emoji variation selector"},
    {0x4e2d, 2, "CJK"}, {0xff21, 2, "fullwidth Latin"},
    {0x1f600, 2, "emoji"}, {0x1fae9, 2, "Unicode 16 face with bags under eyes"},
    {0x1fa89, 2, "Unicode 16 harp"}, {0x20000, 2, "supplementary CJK"}
  };
  int failures = 0;
  for (const auto &f : fixtures) {
    const int old_width = legacy_mosh_wcwidth(f.cp);
    const int current = mosh_wcwidth(f.cp);
    std::printf("U+%04X %-42s patch=%d active=%d expected=%d\n",
                unsigned(f.cp), f.name, old_width, current, f.width);
    if (old_width != f.width || current != f.width) ++failures;
  }
  std::printf("utf8proc %s; Unicode %s\n", utf8proc_version(), utf8proc_unicode_version());
  return failures ? 1 : 0;
}
