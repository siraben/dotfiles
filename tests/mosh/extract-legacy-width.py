"""Extract the complete newly added implementation from the retained patch."""
import pathlib
import sys

patch = pathlib.Path(sys.argv[1]).read_text()
section = patch.split("+++ b/src/terminal/moshwcwidth.cc", 1)[1].split("\n--- ", 1)[0]
lines = section.splitlines()[1:]
assert lines[0].startswith("@@ -0,0 +1,"), "Expected a complete new-file hunk"
source = "\n".join(line[1:] for line in lines[1:] if line.startswith("+")) + "\n"
assert "int mosh_wcwidth(" in source
pathlib.Path(sys.argv[2]).write_text(source)
