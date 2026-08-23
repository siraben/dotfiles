/**
 * Block expensive filesystem scans and disallowed package installation.
 *
 * pi has no hooks.json; the equivalent of a Claude/Codex PreToolUse hook is the
 * `tool_call` extension event, which can return {block, reason}. This mirrors
 * ~/.claude/hooks/block-find-nix-store.sh and ~/.codex/hooks/block-find-nix-store.sh,
 * and additionally covers root-wide scans (`find / ...`), which are what actually
 * burn minutes of wall clock on this machine.
 *
 * Covers both the bash tool and pi's native find/grep tools.
 */

import type { ExtensionAPI, ToolCallEvent } from "@earendil-works/pi-coding-agent";

/** Search roots that are never acceptable to walk recursively. */
const EXPENSIVE_ROOTS = new Set(["/", "/nix", "/nix/store"]);

/** Commands that recursively walk a directory tree. */
const SCANNERS = new Set(["find", "rg", "fd", "fdfind", "ripgrep"]);

/** Prefixes to skip when locating the real command in a segment. */
const PREFIXES = new Set(["sudo", "command", "env", "time", "nice", "ionice", "nohup", "xargs"]);

const SCAN_REASON =
	"Refusing a recursive scan rooted at / or /nix/store: these walk millions of paths " +
	"and take minutes. Scope the search to a project directory, or use nix path-info / " +
	"nix log / a specific known store path instead.";

const BREW_REASON =
	"Refusing Homebrew package changes. Do not recommend that the user run them. " +
	"Add the dependency to an existing project flake.nix; if none exists, ask how to proceed.";

function normalize(token: string): string {
	// Strip shell quoting and trailing punctuation the tokenizer may carry along.
	let value = token.replace(/^['"`]+/, "").replace(/['"`,;]+$/, "");
	if (value.length > 1 && value.endsWith("/") && value !== "/") {
		value = value.slice(0, -1);
	}
	return value;
}

function basename(token: string): string {
	const value = normalize(token);
	const index = value.lastIndexOf("/");
	return index === -1 ? value : value.slice(index + 1);
}

/** Split a command line into pipeline/list segments so each simple command is checked alone. */
function segments(command: string): string[][] {
	const parts: string[][] = [];
	let current: string[] = [];
	let token = "";
	let quote: string | null = null;

	const pushToken = () => {
		if (token) {
			current.push(token);
			token = "";
		}
	};
	const pushSegment = () => {
		pushToken();
		if (current.length) parts.push(current);
		current = [];
	};

	for (let i = 0; i < command.length; i++) {
		const ch = command[i];
		if (quote) {
			if (ch === quote) quote = null;
			else token += ch;
			continue;
		}
		if (ch === '"' || ch === "'" || ch === "`") {
			quote = ch;
			continue;
		}
		if (ch === "\\" && i + 1 < command.length) {
			token += command[++i];
			continue;
		}
		if (ch === ";" || ch === "|" || ch === "&" || ch === "\n") {
			pushSegment();
			continue;
		}
		if (ch === "(" || ch === ")") {
			pushSegment();
			continue;
		}
		if (/\s/.test(ch)) {
			pushToken();
			continue;
		}
		token += ch;
	}
	pushSegment();
	return parts;
}

function scansExpensiveRoot(command: string): boolean {
	for (const segment of segments(command)) {
		let index = 0;
		// Skip sudo/env/... wrappers and VAR=value assignments.
		while (index < segment.length) {
			const candidate = basename(segment[index]);
			if (PREFIXES.has(candidate) || /^[A-Za-z_][A-Za-z0-9_]*=/.test(normalize(segment[index]))) {
				index++;
				continue;
			}
			break;
		}
		if (index >= segment.length) continue;

		const command_name = basename(segment[index]);
		const isGrepRecursive =
			command_name === "grep" && segment.slice(index + 1).some((arg) => /^-[a-zA-Z]*[rR]/.test(normalize(arg)));
		if (!SCANNERS.has(command_name) && !isGrepRecursive) continue;

		for (const argument of segment.slice(index + 1)) {
			if (EXPENSIVE_ROOTS.has(normalize(argument))) return true;
		}
	}
	return false;
}

// Mirrors the brew pattern in the shared Claude/Codex hook script.
const BREW_PATTERN =
	/(^|[;&|`]|\$\()\s*((\/[^\s;&|]+\/)?(zsh|bash|sh)\s+-[^\s]*c\s+["]?)?((command|(\/usr\/bin\/)?env)\s+)?([^\s;&|]+\/)?brew\s+((install|reinstall|upgrade)([\s"]|$)|bundle([\s"]|$))/;

export default function (pi: ExtensionAPI) {
	pi.on("tool_call", (event: ToolCallEvent) => {
		if (event.toolName === "bash") {
			const command = String((event.input as { command?: unknown }).command ?? "");
			if (scansExpensiveRoot(command)) return { block: true, reason: SCAN_REASON };
			if (BREW_PATTERN.test(command)) return { block: true, reason: BREW_REASON };
			return;
		}

		// pi's own find/grep tools take a `path` rather than a shell command.
		if (event.toolName === "find" || event.toolName === "grep") {
			const path = normalize(String((event.input as { path?: unknown }).path ?? ""));
			if (EXPENSIVE_ROOTS.has(path)) return { block: true, reason: SCAN_REASON };
		}
	});
}
