# Command wrapper and policy checks

Run with Python 3.8+ and Node 24:

```sh
python3 -m unittest discover -s tests -p 'test_*.py'
node --experimental-strip-types --test tests/command-policy.test.ts
```

The switch tests replace `uname`, `nix`, and `home-manager` with fake commands;
they never activate a configuration. The hook tests submit command strings as
JSON input and inspect the policy response; they never execute those strings.

`command-policy.json` is shared by the Claude/Codex Python hook and Pi's
TypeScript event handler. Separate expected decisions record existing parser
differences (shell recursion, recursive grep, and command-position detection).
Changing those decisions is a policy change, separate from extracting the hook.
