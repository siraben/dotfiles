/**
 * Best-effort guard against accidentally expensive filesystem scans and
 * Homebrew mutations. This reduces common mistakes; it is not a shell sandbox.
 * Dynamic expansion, sourced scripts, aliases, and arbitrary interpreters can
 * always evade command-string inspection.
 */

import { posix as path } from "node:path";
import type { ExtensionAPI, ToolCallEvent } from "@earendil-works/pi-coding-agent";

const EXPENSIVE_ROOTS = new Set(["/", "/nix", "/nix/store"]);
const SCANNERS = new Set(["find", "rg", "ripgrep", "fd", "fdfind"]);
const SHELLS = new Set(["sh", "bash", "zsh"]);
const PREFIXES = new Set(["sudo", "command", "env", "time", "nice", "ionice", "nohup", "xargs"]);
const BREW_MUTATIONS = new Set(["install", "reinstall", "upgrade", "uninstall", "remove", "bundle"]);

const SCAN_REASON =
	"Refusing a recursive scan rooted at / or /nix/store: these walk millions of paths " +
	"and take minutes. Scope the search to a project directory, or use nix path-info / " +
	"nix log / a specific known store path instead.";

const BREW_REASON =
	"Refusing a Homebrew package change. Do not recommend that the user run it. " +
	"Add the dependency to an existing project flake.nix; if none exists, ask how to proceed.";

type ParsedCommand = {
	segments: string[][];
	nested: string[];
};

type SimpleCommand = {
	name: string;
	args: string[];
};

function normalizeToken(token: string): string {
	return token.replace(/^["']+/, "").replace(/["',;]+$/, "");
}

function commandBasename(token: string): string {
	const value = normalizeToken(token);
	const index = value.lastIndexOf("/");
	return index === -1 ? value : value.slice(index + 1);
}

function normalizeAbsolutePath(token: string): string | undefined {
	const value = normalizeToken(token);
	if (!value.startsWith("/")) return undefined;
	return path.normalize(value);
}

function isExpensiveRoot(token: string): boolean {
	const normalized = normalizeAbsolutePath(token);
	return normalized !== undefined && EXPENSIVE_ROOTS.has(normalized);
}

/**
 * Tokenize enough shell syntax to identify simple pipeline/list commands while
 * preserving quoted `sh -c` payloads. Backtick substitutions are returned as
 * nested commands. Parentheses, braces, and `$()` naturally become separate
 * segments.
 */
function parseCommandLine(command: string): ParsedCommand {
	const segments: string[][] = [];
	const nested: string[] = [];
	let current: string[] = [];
	let token = "";
	let quote: "'" | '"' | null = null;

	const pushToken = () => {
		if (token) current.push(token);
		token = "";
	};
	const pushSegment = () => {
		pushToken();
		if (current.length) segments.push(current);
		current = [];
	};

	for (let i = 0; i < command.length; i++) {
		const character = command[i];
		if (quote) {
			if (character === quote) {
				quote = null;
			} else if (quote === '"' && character === "`") {
				let subcommand = "";
				for (i++; i < command.length && command[i] !== "`"; i++) {
					if (command[i] === "\\" && i + 1 < command.length) subcommand += command[++i];
					else subcommand += command[i];
				}
				nested.push(subcommand);
				token += "__command_substitution__";
			} else if (quote === '"' && character === "$" && command[i + 1] === "(") {
				let depth = 1;
				let subcommand = "";
				for (i += 2; i < command.length && depth > 0; i++) {
					if (command[i] === "(") depth++;
					else if (command[i] === ")") depth--;
					if (depth > 0) subcommand += command[i];
				}
				i--;
				nested.push(subcommand);
				token += "__command_substitution__";
			} else if (character === "\\" && quote === '"' && i + 1 < command.length) {
				token += command[++i];
			} else {
				token += character;
			}
			continue;
		}
		if (character === "'" || character === '"') {
			quote = character;
			continue;
		}
		if (character === "`") {
			let subcommand = "";
			for (i++; i < command.length && command[i] !== "`"; i++) {
				if (command[i] === "\\" && i + 1 < command.length) subcommand += command[++i];
				else subcommand += command[i];
			}
			nested.push(subcommand);
			token += "__command_substitution__";
			continue;
		}
		if (character === "\\" && i + 1 < command.length) {
			token += command[++i];
			continue;
		}
		if (character === ";" || character === "|" || character === "&" || character === "\n") {
			pushSegment();
			continue;
		}
		if (
			character === "(" ||
			character === ")" ||
			character === "{" ||
			character === "}"
		) {
			pushSegment();
			continue;
		}
		if (/\s/.test(character)) {
			pushToken();
			continue;
		}
		token += character;
	}
	pushSegment();
	return { segments, nested };
}

function isAssignment(token: string): boolean {
	return /^[A-Za-z_][A-Za-z0-9_]*=/.test(normalizeToken(token));
}

const PREFIX_OPTIONS_WITH_VALUES: Record<string, Set<string>> = {
	sudo: new Set(["-C", "-D", "-g", "-h", "-p", "-R", "-T", "-u", "--chdir", "--group", "--host", "--prompt", "--role", "--type", "--user"]),
	env: new Set(["-C", "-S", "-u", "--chdir", "--split-string", "--unset"]),
	nice: new Set(["-n", "--adjustment"]),
	ionice: new Set(["-c", "-n", "-p", "-P", "-u", "--class", "--classdata", "--pid", "--pgid", "--uid"]),
	xargs: new Set(["-a", "-d", "-E", "-I", "-L", "-n", "-P", "-s", "--arg-file", "--delimiter", "--eof", "--max-args", "--max-chars", "--max-lines", "--max-procs", "--replace"]),
};

function optionName(token: string): string {
	return token.includes("=") ? token.slice(0, token.indexOf("=")) : token;
}

function skipPrefixOptions(name: string, tokens: string[], start: number): number {
	let index = start;
	const optionsWithValues = PREFIX_OPTIONS_WITH_VALUES[name] ?? new Set<string>();
	while (index < tokens.length) {
		const token = normalizeToken(tokens[index]);
		if (token === "--") return index + 1;
		if (!token.startsWith("-") || token === "-") break;
		const option = optionName(token);
		const attachedShortValue = option.length > 2 && !option.startsWith("--");
		index++;
		if (optionsWithValues.has(option) && !token.includes("=") && !attachedShortValue) index++;
	}
	return index;
}

function unwrapPrefixes(tokens: string[]): SimpleCommand | undefined {
	let index = 0;
	while (index < tokens.length && isAssignment(tokens[index])) index++;
	while (index < tokens.length && PREFIXES.has(commandBasename(tokens[index]))) {
		const prefix = commandBasename(tokens[index++]);
		index = skipPrefixOptions(prefix, tokens, index);
		if (prefix === "env") while (index < tokens.length && isAssignment(tokens[index])) index++;
	}
	if (index >= tokens.length) return undefined;
	return {
		name: commandBasename(tokens[index]),
		args: tokens.slice(index + 1).map(normalizeToken),
	};
}

function shellPayload(command: SimpleCommand): string | undefined {
	if (!SHELLS.has(command.name)) return undefined;
	for (let index = 0; index < command.args.length; index++) {
		const argument = command.args[index];
		if (argument === "--command" || argument === "-c" || (/^-[^-]+$/.test(argument) && argument.slice(1).includes("c"))) {
			return command.args[index + 1];
		}
	}
	return undefined;
}

function optionPositionals(
	args: string[],
	optionsWithValues: Set<string>,
	onOption?: (name: string, value: string | undefined) => void,
): string[] {
	const positionals: string[] = [];
	for (let index = 0; index < args.length; index++) {
		const argument = args[index];
		if (argument === "--") {
			positionals.push(...args.slice(index + 1));
			break;
		}
		if (argument.startsWith("-") && argument !== "-") {
			let name = optionName(argument);
			let value: string | undefined;
			if (argument.includes("=")) {
				value = argument.slice(argument.indexOf("=") + 1);
			} else {
				const attached = [...optionsWithValues].find(
					(option) => option.length === 2 && argument.startsWith(option) && argument.length > 2,
				);
				if (attached) {
					name = attached;
					value = argument.slice(attached.length);
				} else if (optionsWithValues.has(name)) {
					value = args[++index];
				}
			}
			onOption?.(name, value);
			continue;
		}
		positionals.push(argument);
	}
	return positionals;
}

const RG_VALUE_OPTIONS = new Set([
	"-A", "-B", "-C", "-E", "-e", "-f", "-g", "-j", "-m", "-M", "-r", "-t", "-T",
	"--after-context", "--before-context", "--colors", "--context", "--context-separator",
	"--dfa-size-limit", "--encoding", "--engine", "--field-context-separator",
	"--field-match-separator", "--glob", "--iglob", "--ignore-file", "--max-columns",
	"--max-count", "--max-depth", "--max-filesize", "--path-separator", "--pre", "--pre-glob",
	"--regexp", "--replace", "--sort", "--sortr", "--threads", "--type", "--type-add", "--type-clear",
]);

function ripgrepScansRoot(args: string[]): boolean {
	let explicitPattern = false;
	let filesMode = false;
	const positionals = optionPositionals(args, RG_VALUE_OPTIONS, (name) => {
		if (name === "-e" || name === "-f" || name === "--regexp") explicitPattern = true;
		if (name === "--files") filesMode = true;
	});
	const roots = filesMode || explicitPattern ? positionals : positionals.slice(1);
	return roots.some(isExpensiveRoot);
}

const FD_VALUE_OPTIONS = new Set([
	"-d", "-E", "-e", "-j", "-t", "-x", "-X",
	"--base-directory", "--changed-before", "--changed-within", "--exact-depth", "--exclude",
	"--exec", "--exec-batch", "--extension", "--format", "--ignore-file", "--max-depth",
	"--max-results", "--min-depth", "--owner", "--path-separator", "--size", "--threads", "--type",
]);

function fdScansRoot(args: string[]): boolean {
	let baseDirectory: string | undefined;
	const positionals = optionPositionals(args, FD_VALUE_OPTIONS, (name, value) => {
		if (name === "--base-directory") baseDirectory = value;
	});
	return (baseDirectory !== undefined && isExpensiveRoot(baseDirectory)) || positionals.slice(1).some(isExpensiveRoot);
}

const GREP_VALUE_OPTIONS = new Set([
	"-A", "-B", "-C", "-D", "-d", "-e", "-f", "-m",
	"--after-context", "--before-context", "--binary-files", "--context", "--devices",
	"--directories", "--exclude", "--exclude-dir", "--exclude-from", "--file", "--include",
	"--label", "--max-count", "--regexp",
]);

function grepScansRoot(args: string[]): boolean {
	let recursive = false;
	let explicitPattern = false;
	const positionals = optionPositionals(args, GREP_VALUE_OPTIONS, (name, value) => {
		if (name === "-e" || name === "-f" || name === "--regexp" || name === "--file") explicitPattern = true;
		if (name === "--directories" && value === "recurse") recursive = true;
		if (/^-[^-]*[rR]/.test(name)) recursive = true;
	});
	if (!recursive) return false;
	const roots = explicitPattern ? positionals : positionals.slice(1);
	return roots.some(isExpensiveRoot);
}

function findScansRoot(args: string[]): boolean {
	let index = 0;
	while (index < args.length && ["-H", "-L", "-P"].includes(args[index])) index++;
	if (args[index] === "--") index++;
	const roots: string[] = [];
	for (; index < args.length; index++) {
		const argument = args[index];
		if (argument.startsWith("-") || argument === "!" || argument === "(") break;
		roots.push(argument);
	}
	return roots.some(isExpensiveRoot);
}

function simpleCommandScansRoot(command: SimpleCommand): boolean {
	if (command.name === "find") return findScansRoot(command.args);
	if (command.name === "rg" || command.name === "ripgrep") return ripgrepScansRoot(command.args);
	if (command.name === "fd" || command.name === "fdfind") return fdScansRoot(command.args);
	if (command.name === "grep") return grepScansRoot(command.args);
	return false;
}

function scansExpensiveRoot(command: string): boolean {
	const parsed = parseCommandLine(command);
	if (parsed.nested.some(scansExpensiveRoot)) return true;
	for (const segment of parsed.segments) {
		const simple = unwrapPrefixes(segment);
		if (!simple) continue;
		const nestedShell = shellPayload(simple);
		if (nestedShell !== undefined && scansExpensiveRoot(nestedShell)) return true;
		if (SCANNERS.has(simple.name) || simple.name === "grep") {
			if (simpleCommandScansRoot(simple)) return true;
		}
	}
	return false;
}

function changesHomebrewPackages(command: string): boolean {
	const parsed = parseCommandLine(command);
	if (parsed.nested.some(changesHomebrewPackages)) return true;
	for (const segment of parsed.segments) {
		const simple = unwrapPrefixes(segment);
		if (!simple) continue;
		const nestedShell = shellPayload(simple);
		if (nestedShell !== undefined && changesHomebrewPackages(nestedShell)) return true;
		if (simple.name !== "brew") continue;
		const subcommand = simple.args.find((argument) => !argument.startsWith("-"));
		if (subcommand && BREW_MUTATIONS.has(subcommand)) return true;
	}
	return false;
}

export default function blockExpensiveScans(pi: ExtensionAPI) {
	pi.on("tool_call", (event: ToolCallEvent) => {
		if (event.toolName === "bash") {
			const command = String((event.input as { command?: unknown }).command ?? "");
			if (scansExpensiveRoot(command)) return { block: true, reason: SCAN_REASON };
			if (changesHomebrewPackages(command)) return { block: true, reason: BREW_REASON };
			return;
		}

		if (event.toolName === "find" || event.toolName === "grep") {
			const root = String((event.input as { path?: unknown }).path ?? "");
			if (isExpensiveRoot(root)) return { block: true, reason: SCAN_REASON };
		}
	});
}
