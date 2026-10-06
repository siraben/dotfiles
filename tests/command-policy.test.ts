import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import test from "node:test";
import register from "../home-manager/.config/nixpkgs/pi-block-expensive-scans.ts";

const fixtures = JSON.parse(readFileSync(new URL("./command-policy.json", import.meta.url), "utf8"));
let callback: (event: unknown) => { block: boolean; reason: string } | undefined;
register({ on(_event: string, handler: typeof callback) { callback = handler; } } as any);
for (const fixture of fixtures) {
  test(fixture.name, () => {
    const result = callback({ toolName: "bash", input: { command: fixture.command } });
    assert.equal(result?.block ?? false, fixture.pi !== "allow");
    if (fixture.pi === "brew") assert.match(result!.reason, /Homebrew/);
    if (fixture.pi === "scan") assert.match(result!.reason, /recursive scan/);
  });
}
for (const toolName of ["find", "grep"]) {
  for (const path of ["/", "/nix", "/nix/store", "/nix/store/"]) {
    test(`${toolName} blocks ${path}`, () => assert.equal(callback({ toolName, input: { path } })?.block, true));
  }
  test(`${toolName} allows project scope`, () => assert.equal(callback({ toolName, input: { path: "/project" } }), undefined));
}
