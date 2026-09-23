// Run: node --experimental-vm-modules --test test-pi-codex-usage.mjs
import assert from "node:assert/strict";
import * as crypto from "node:crypto";
import * as os from "node:os";
import * as path from "node:path";
import * as fs from "node:fs/promises";
import { stripTypeScriptTypes } from "node:module";
import { test } from "node:test";
import { SourceTextModule, SyntheticModule } from "node:vm";

async function loadExtension() {
	const source = stripTypeScriptTypes(
		await fs.readFile(new URL("./pi-codex-usage.ts", import.meta.url), "utf8"),
	);
	const imports = {
		"node:crypto": crypto,
		"node:os": os,
		"node:path": path,
		"@earendil-works/pi-coding-agent": {
			getAgentDir: () => "/tmp/pi-codex-usage-test",
			readStoredCredential: () => undefined,
		},
		"@earendil-works/pi-tui": {
			truncateToWidth: (value, width) => value.slice(0, width),
			visibleWidth: (value) => value.length,
		},
	};
	const module = new SourceTextModule(source);
	await module.link((specifier) => {
		const exports = imports[specifier];
		assert.ok(exports, `Unexpected import: ${specifier}`);
		return new SyntheticModule(Object.keys(exports), function () {
			for (const [key, value] of Object.entries(exports)) this.setExport(key, value);
		});
	});
	await module.evaluate();
	return module.namespace.default;
}

function fixture(extension) {
	const commands = new Map();
	const renderers = new Map();
	const entries = [];
	extension({
		appendEntry: (type, data) => entries.push({ type, data }),
		getSessionName: () => undefined,
		getThinkingLevel: () => "high",
		registerCommand: (name, command) => commands.set(name, command),
		registerEntryRenderer: (name, renderer) => renderers.set(name, renderer),
	});
	return { commands, renderers, entries };
}

test("registers only the portable status and usage commands", async () => {
	const registered = fixture(await loadExtension());
	assert.deepEqual([...registered.commands.keys()], ["status", "usage"]);
	assert.deepEqual([...registered.renderers.keys()], ["codex-status", "codex-usage"]);
	assert.equal(registered.commands.has("codex-profile"), false);
});

test("commands reject non-Codex models before accessing credentials or the network", async () => {
	const registered = fixture(await loadExtension());
	const notifications = [];
	const ctx = {
		model: { provider: "anthropic", id: "claude", contextWindow: 1 },
		modelRegistry: {
			isUsingOAuth: () => { throw new Error("must not resolve auth"); },
		},
		ui: {
			notify: (message, level) => notifications.push({ message, level }),
		},
		waitForIdle: async () => {},
	};
	await registered.commands.get("status").handler("", ctx);
	await registered.commands.get("usage").handler("show", ctx);
	assert.equal(notifications.length, 2);
	assert.ok(notifications.every(({ message, level }) =>
		level === "warning" && message.includes("active OpenAI Codex model")));
});
