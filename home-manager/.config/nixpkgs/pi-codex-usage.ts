import { randomUUID } from "node:crypto";
import { homedir } from "node:os";
import { join } from "node:path";
import {
	getAgentDir,
	readStoredCredential,
	type ExtensionAPI,
	type ExtensionCommandContext,
	type ExtensionContext,
	type Theme,
} from "@earendil-works/pi-coding-agent";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";

const PROVIDER_ID = "openai-codex";

type JsonObject = Record<string, unknown>;

type PiCredential = {
	type: "oauth";
	access: string;
	refresh: string;
	expires: number;
	accountId: string;
};

// Usage parsing mirrors OpenAI Codex CLI's backend client. Rendering below
// intentionally follows Pi's section-and-label style instead of Codex's card.
type RateLimitWindowPayload = {
	used_percent?: number;
	limit_window_seconds?: number;
	reset_at?: number;
};

type RateLimitPayload = {
	primary_window?: RateLimitWindowPayload | null;
	secondary_window?: RateLimitWindowPayload | null;
};

type ResetCreditsSummary = {
	available_count?: number;
	applicable_available_count?: number;
};

type ResetCredit = {
	id: string;
	reset_type?: string;
	status?: string;
	granted_at?: string;
	expires_at?: string | null;
	title?: string | null;
	description?: string | null;
};

type ResetCreditsDetails = {
	available_count?: number;
	credits?: ResetCredit[];
};

type ConsumeResetResponse = {
	code?: "reset" | "nothing_to_reset" | "no_credit" | "already_redeemed" | string;
	windows_reset?: number;
};

type UsagePayload = {
	email?: string;
	plan_type?: string;
	rate_limit?: RateLimitPayload | null;
	rate_limit_reset_credits?: ResetCreditsSummary | null;
	additional_rate_limits?: Array<{
		limit_name?: string;
		metered_feature?: string;
		rate_limit?: RateLimitPayload | null;
	}> | null;
	credits?: {
		has_credits?: boolean;
		unlimited?: boolean;
		balance?: string | number | null;
	} | null;
	spend_control?: {
		individual_limit?: {
			limit?: string | number;
			used?: string | number;
			remaining_percent?: number;
			reset_at?: number;
		} | null;
	} | null;
};

type StatusLimitRow = {
	label: string;
	text?: string;
	percentUsed?: number;
	resetsAt?: number;
	details?: string;
};

type CodexStatusEntryData = {
	provider: string;
	model: string;
	thinking: string;
	directory: string;
	account?: string;
	authExpires?: number;
	threadName?: string;
	session?: string;
	contextTokens: number;
	contextWindow: number;
	limits: StatusLimitRow[];
	limitsAvailable: boolean;
};

type UsageCreditDisplay = {
	title: string;
	expires: string;
	description?: string;
};

type CodexUsageEntryData = {
	account: string;
	limits: StatusLimitRow[];
	limitsAvailable: boolean;
	bankedResets?: number;
	applicableResets?: number;
	resetCredits: UsageCreditDisplay[];
	resetDetailsAvailable: boolean;
};

type CodexRequestAuth = {
	token: string;
	accountId: string;
	baseUrl: string;
	chatGptStyle: boolean;
};

const piAuthPath = join(getAgentDir(), "auth.json");

function isObject(value: unknown): value is JsonObject {
	return typeof value === "object" && value !== null && !Array.isArray(value);
}

function decodeJwt(token: string): JsonObject | undefined {
	try {
		const payload = token.split(".")[1];
		if (!payload) return undefined;
		const normalized = payload.replace(/-/g, "+").replace(/_/g, "/");
		const padded = normalized.padEnd(Math.ceil(normalized.length / 4) * 4, "=");
		const parsed: unknown = JSON.parse(Buffer.from(padded, "base64").toString("utf8"));
		return isObject(parsed) ? parsed : undefined;
	} catch {
		return undefined;
	}
}

function jwtTime(token: string, claim: "exp" | "iat"): number | undefined {
	const value = decodeJwt(token)?.[claim];
	return typeof value === "number" && Number.isFinite(value) ? value * 1000 : undefined;
}

function parsePiCredential(value: unknown): PiCredential | undefined {
	if (
		!isObject(value) ||
		value.type !== "oauth" ||
		typeof value.access !== "string" ||
		typeof value.refresh !== "string" ||
		typeof value.expires !== "number" ||
		!Number.isFinite(value.expires) ||
		typeof value.accountId !== "string"
	) {
		return undefined;
	}
	return value as PiCredential;
}

function currentPiCredential(): PiCredential | undefined {
	return parsePiCredential(readStoredCredential(PROVIDER_ID, piAuthPath));
}

function jwtAccountId(token: string): string | undefined {
	const auth = decodeJwt(token)?.["https://api.openai.com/auth"];
	return isObject(auth) && typeof auth.chatgpt_account_id === "string" ? auth.chatgpt_account_id : undefined;
}

function titleCase(value: string): string {
	return value
		.split(/[_\s-]+/)
		.filter(Boolean)
		.map((word) => word[0]?.toUpperCase() + word.slice(1).toLowerCase())
		.join(" ");
}

function planDisplayName(plan?: string): string | undefined {
	if (!plan) return undefined;
	const normalized = plan.toLowerCase();
	if (["team", "self_serve_business_pro_lite", "self_serve_business_prolite", "self_serve_business_usage_based"].includes(normalized)) {
		return "Business";
	}
	if (["business", "enterprise_cbp_usage_based", "enterprise"].includes(normalized)) return "Enterprise";
	if (normalized === "enterprise_cbp_automation") return "Enterprise (Automation)";
	if (normalized === "pro_lite" || normalized === "prolite") return "Pro Lite";
	if (normalized === "edu_plus") return "Edu Plus";
	if (normalized === "edu_pro") return "Edu Pro";
	return titleCase(normalized);
}

function approximateWindow(minutes: number, expected: number): boolean {
	return minutes >= expected * 0.95 && minutes <= expected * 1.05;
}

function limitDuration(windowSeconds: number | undefined, secondary: boolean): string {
	if (typeof windowSeconds !== "number" || !Number.isFinite(windowSeconds)) {
		return secondary ? "secondary usage" : "usage";
	}
	const minutes = Math.max(0, windowSeconds / 60);
	if (approximateWindow(minutes, 5 * 60)) return "5h";
	if (approximateWindow(minutes, 24 * 60)) return "daily";
	if (approximateWindow(minutes, 7 * 24 * 60)) return "weekly";
	if (approximateWindow(minutes, 30 * 24 * 60)) return "monthly";
	if (approximateWindow(minutes, 365 * 24 * 60)) return "annual";
	return secondary ? "secondary usage" : "usage";
}

function addWindowRows(
	rows: StatusLimitRow[],
	limitName: string,
	limit: RateLimitPayload | null | undefined,
): void {
	if (!limit) return;
	const windows = [
		{ value: limit.primary_window, secondary: false },
		{ value: limit.secondary_window, secondary: true },
	].filter((entry): entry is { value: RateLimitWindowPayload; secondary: boolean } => !!entry.value);
	if (windows.length === 0) return;

	const isCodex = limitName.toLowerCase() === "codex";
	if (!isCodex && windows.length > 1) rows.push({ label: `${limitName} limit`, text: "" });
	for (const { value, secondary } of windows) {
		if (typeof value.used_percent !== "number" || !Number.isFinite(value.used_percent)) continue;
		const duration = titleCase(limitDuration(value.limit_window_seconds, secondary));
		const label = !isCodex && windows.length === 1
			? `${limitName} ${duration} limit`
			: `${duration} limit`;
		rows.push({
			label,
			percentUsed: value.used_percent,
			resetsAt: typeof value.reset_at === "number" ? value.reset_at : undefined,
		});
	}
}

function roundedCreditAmount(value: unknown): string | undefined {
	const amount = typeof value === "number" ? value : typeof value === "string" ? Number(value.trim()) : Number.NaN;
	return Number.isFinite(amount) && amount >= 0 ? Math.round(amount).toLocaleString("en-US") : undefined;
}

function composeLimitRows(payload: UsagePayload): StatusLimitRow[] {
	const rows: StatusLimitRow[] = [];
	addWindowRows(rows, "codex", payload.rate_limit);
	for (const additional of payload.additional_rate_limits || []) {
		const name = additional.limit_name?.trim() || additional.metered_feature?.trim() || "codex-other";
		addWindowRows(rows, name, additional.rate_limit);
	}

	if (payload.credits?.unlimited) {
		rows.push({ label: "Credits", text: "Unlimited" });
	} else if (payload.credits?.has_credits) {
		const balance = roundedCreditAmount(payload.credits.balance);
		rows.push({ label: "Credits", text: balance && Number(balance.replaceAll(",", "")) > 0 ? `${balance} credits` : "Available" });
	}

	const individual = payload.spend_control?.individual_limit;
	if (individual && typeof individual.remaining_percent === "number") {
		const used = roundedCreditAmount(individual.used);
		const limit = roundedCreditAmount(individual.limit);
		rows.push({
			label: "Monthly credit limit",
			percentUsed: 100 - individual.remaining_percent,
			resetsAt: typeof individual.reset_at === "number" ? individual.reset_at : undefined,
			details: used && limit ? `${used} of ${limit} credits used` : undefined,
		});
	}

	const resetSummary = payload.rate_limit_reset_credits;
	if (resetSummary && typeof resetSummary.available_count === "number") {
		const count = Math.max(0, Math.floor(resetSummary.available_count));
		const applicable = typeof resetSummary.applicable_available_count === "number"
			? Math.max(0, Math.floor(resetSummary.applicable_available_count))
			: undefined;
		rows.push({
			label: "Banked resets",
			text: `${count} available${applicable !== undefined ? ` (${applicable} usable now)` : ""}`,
		});
	}
	return rows;
}

async function resolveCodexOAuth(ctx: ExtensionContext): Promise<CodexRequestAuth> {
	// Check Pi's persisted credential first. This prevents an API key supplied by
	// the environment from making /status or /usage look like an OAuth login.
	if (!currentPiCredential()) throw new Error("Pi has no OpenAI Codex OAuth credential configured");
	const resolved = await ctx.modelRegistry.getProviderAuth(PROVIDER_ID);
	const token = resolved?.auth.apiKey;
	if (!token) throw new Error("No OpenAI Codex OAuth credential configured");
	const accountId = jwtAccountId(token);
	if (!accountId) throw new Error("OpenAI Codex token does not contain an account ID");

	const providerBaseUrl = ctx.modelRegistry.getProvider(PROVIDER_ID)?.baseUrl;
	const baseUrl = (resolved.auth.baseUrl || providerBaseUrl || "https://chatgpt.com/backend-api").replace(/\/+$/, "");
	return { token, accountId, baseUrl, chatGptStyle: baseUrl.includes("/backend-api") };
}

function codexEndpoint(auth: CodexRequestAuth, chatGptPath: string, codexPath: string): string {
	return `${auth.baseUrl}${auth.chatGptStyle ? chatGptPath : codexPath}`;
}

async function requestCodexJson<T>(
	auth: CodexRequestAuth,
	url: string,
	requestName: string,
	init: RequestInit = {},
): Promise<T> {
	const controller = new AbortController();
	const timeout = setTimeout(() => controller.abort(), 15_000);
	const headers = new Headers(init.headers);
	headers.set("Authorization", `Bearer ${auth.token}`);
	headers.set("ChatGPT-Account-Id", auth.accountId);
	headers.set("User-Agent", "codex-cli");
	if (init.body) headers.set("Content-Type", "application/json");
	try {
		const response = await fetch(url, {
			...init,
			headers,
			signal: controller.signal,
		});
		if (!response.ok) throw new Error(`${requestName} failed (${response.status})`);
		const payload: unknown = await response.json();
		if (!isObject(payload)) throw new Error(`${requestName} returned an invalid response`);
		return payload as T;
	} finally {
		clearTimeout(timeout);
	}
}

async function fetchCodexUsageWithAuth(auth: CodexRequestAuth): Promise<UsagePayload> {
	return requestCodexJson(
		auth,
		codexEndpoint(auth, "/wham/usage", "/api/codex/usage"),
		"Codex usage request",
	);
}

async function fetchResetCredits(auth: CodexRequestAuth): Promise<ResetCreditsDetails> {
	return requestCodexJson(
		auth,
		codexEndpoint(auth, "/wham/rate-limit-reset-credits", "/api/codex/rate-limit-reset-credits"),
		"Codex reset-credit request",
	);
}

async function consumeResetCredit(
	auth: CodexRequestAuth,
	idempotencyKey: string,
	creditId?: string,
): Promise<ConsumeResetResponse> {
	return requestCodexJson(
		auth,
		codexEndpoint(auth, "/wham/rate-limit-reset-credits/consume", "/api/codex/rate-limit-reset-credits/consume"),
		"Codex reset-credit redemption",
		{
			method: "POST",
			body: JSON.stringify({
				redeem_request_id: idempotencyKey,
				...(creditId ? { credit_id: creditId } : {}),
			}),
		},
	);
}

function compactTokens(value: number): string {
	const safe = Math.max(0, Math.round(value));
	if (safe < 1_000) return String(safe);
	const scales: Array<[number, string]> = [
		[1_000_000_000_000, "T"],
		[1_000_000_000, "B"],
		[1_000_000, "M"],
		[1_000, "K"],
	];
	const [divisor, suffix] = scales.find(([divisor]) => safe >= divisor) || scales[scales.length - 1];
	const scaled = safe / divisor;
	const decimals = scaled < 10 ? 2 : scaled < 100 ? 1 : 0;
	return `${scaled.toFixed(decimals).replace(/\.0+$|(?<=\.[0-9])0+$/, "")}${suffix}`;
}

function formatResetTimestamp(timestamp?: number): string | undefined {
	if (typeof timestamp !== "number" || !Number.isFinite(timestamp)) return undefined;
	const reset = new Date(timestamp * 1_000);
	if (Number.isNaN(reset.getTime())) return undefined;
	const now = new Date();
	const time = reset.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit", hour12: false });
	if (reset.getFullYear() === now.getFullYear() && reset.getMonth() === now.getMonth() && reset.getDate() === now.getDate()) {
		return time;
	}
	return `${time} on ${reset.getDate()} ${reset.toLocaleDateString("en-US", { month: "short" })}`;
}

function formatDirectory(path: string): string {
	const home = homedir();
	return path === home ? "~" : path.startsWith(`${home}/`) ? `~/${path.slice(home.length + 1)}` : path;
}

function wrapPlain(text: string, width: number): string[] {
	if (width <= 0) return [""];
	if (!text) return [""];
	const lines: string[] = [];
	let remaining = text;
	while (visibleWidth(remaining) > width) {
		let end = Math.min(remaining.length, width);
		while (end > 0 && visibleWidth(remaining.slice(0, end)) > width) end--;
		const breakAt = remaining.lastIndexOf(" ", end);
		if (breakAt > 0) end = breakAt;
		if (end <= 0) end = 1;
		lines.push(remaining.slice(0, end));
		remaining = remaining.slice(end).trimStart();
	}
	lines.push(remaining);
	return lines;
}

function renderCodexStatus(data: CodexStatusEntryData, width: number, theme: Theme): string[] {
	if (width < 8) return [truncateToWidth("Status", width)];
	const availableWidth = Math.max(1, width - 2);
	const lines: string[] = [theme.bold("Provider Status")];

	const addSection = (heading: string, rows: Array<{ label: string; value: string; details?: string }>) => {
		lines.push("", theme.bold(heading));
		const labelWidth = Math.max(...rows.map((row) => visibleWidth(row.label)));
		const valueWidth = Math.max(1, availableWidth - labelWidth - 2);
		for (const row of rows) {
			const chunks = wrapPlain(row.value, valueWidth);
			const prefix = `${row.label}:`.padEnd(labelWidth + 2);
			lines.push(`${theme.fg("dim", prefix)}${chunks[0] || ""}`);
			for (const chunk of chunks.slice(1)) lines.push(`${" ".repeat(labelWidth + 2)}${chunk}`);
			if (row.details) {
				for (const chunk of wrapPlain(row.details, valueWidth)) {
					lines.push(`${" ".repeat(labelWidth + 2)}${theme.fg("dim", chunk)}`);
				}
			}
		}
	};

	const runtimeRows = [
		{ label: "Provider", value: data.provider },
		{ label: "Model", value: data.model },
		{ label: "Thinking", value: data.thinking },
		{ label: "Directory", value: data.directory },
	];
	if (data.threadName) runtimeRows.push({ label: "Name", value: data.threadName });
	if (data.session) runtimeRows.push({ label: "Session", value: data.session });
	addSection("Runtime", runtimeRows);

	const authRows = [
		{ label: "Type", value: "OAuth" },
		{ label: "Store", value: "~/.pi/agent/auth.json" },
	];
	if (data.account) authRows.push({ label: "Account", value: data.account });
	if (data.authExpires) authRows.push({ label: "Expires", value: formatExpiry(data.authExpires) });
	addSection("Authentication", authRows);

	const usedPercent = Math.max(0, Math.min(100, (data.contextTokens / data.contextWindow) * 100));
	addSection("Context", [
		{ label: "Used", value: `${compactTokens(data.contextTokens)} (${usedPercent.toFixed(1)}%)` },
		{ label: "Window", value: compactTokens(data.contextWindow) },
		{ label: "Remaining", value: `${(100 - usedPercent).toFixed(1)}%` },
	]);

	const limitRows: Array<{ label: string; value: string; details?: string }> = [];
	if (!data.limitsAvailable || data.limits.length === 0) {
		limitRows.push({ label: "Limits", value: "Not available for this account" });
	} else {
		const labelWidth = Math.max(...data.limits.map((row) => visibleWidth(row.label)));
		const valueWidth = Math.max(1, availableWidth - labelWidth - 2);
		for (const row of data.limits) {
			if (typeof row.percentUsed !== "number") {
				limitRows.push({ label: row.label, value: row.text || "" });
				continue;
			}
			const remaining = Math.max(0, Math.min(100, 100 - row.percentUsed));
			const filled = Math.min(20, Math.round(remaining / 5));
			const bar = `[${"█".repeat(filled)}${"░".repeat(20 - filled)}]`;
			const summary = `${remaining.toFixed(0)}% left`;
			const reset = formatResetTimestamp(row.resetsAt);
			const full = `${bar} ${summary}${reset ? ` (resets ${reset})` : ""}`;
			limitRows.push({
				label: row.label,
				value: visibleWidth(full) <= valueWidth ? full : `${summary}${reset ? ` (resets ${reset})` : ""}`,
				details: row.details,
			});
		}
	}
	limitRows.push({ label: "Details", value: "https://chatgpt.com/codex/settings/usage" });
	addSection("Usage Limits", limitRows);

	return lines.map((line) => truncateToWidth(` ${line}`, width, ""));
}

function renderCodexUsage(data: CodexUsageEntryData, width: number, theme: Theme): string[] {
	if (width < 8) return [truncateToWidth("Usage", width)];
	const availableWidth = Math.max(1, width - 2);
	const lines: string[] = [theme.bold("Account Usage")];

	const addSection = (heading: string, rows: Array<{ label: string; value: string; details?: string }>) => {
		lines.push("", theme.bold(heading));
		const labelWidth = Math.max(...rows.map((row) => visibleWidth(row.label)));
		const valueWidth = Math.max(1, availableWidth - labelWidth - 2);
		for (const row of rows) {
			const chunks = wrapPlain(row.value, valueWidth);
			const prefix = `${row.label}:`.padEnd(labelWidth + 2);
			lines.push(`${theme.fg("dim", prefix)}${chunks[0] || ""}`);
			for (const chunk of chunks.slice(1)) lines.push(`${" ".repeat(labelWidth + 2)}${chunk}`);
			if (row.details) {
				for (const chunk of wrapPlain(row.details, valueWidth)) {
					lines.push(`${" ".repeat(labelWidth + 2)}${theme.fg("dim", chunk)}`);
				}
			}
		}
	};

	const accountRows = [{ label: "Account", value: data.account }];
	addSection("Authentication", accountRows);

	const usageRows: Array<{ label: string; value: string; details?: string }> = [];
	if (!data.limitsAvailable || data.limits.length === 0) {
		usageRows.push({ label: "Limits", value: "Not available for this account" });
	} else {
		const labelWidth = Math.max(...data.limits.map((row) => visibleWidth(row.label)));
		const valueWidth = Math.max(1, availableWidth - labelWidth - 2);
		for (const row of data.limits) {
			if (typeof row.percentUsed !== "number") {
				// Reset counts have their own section below.
				if (row.label !== "Banked resets") usageRows.push({ label: row.label, value: row.text || "" });
				continue;
			}
			const remaining = Math.max(0, Math.min(100, 100 - row.percentUsed));
			const filled = Math.min(20, Math.round(remaining / 5));
			const bar = `[${"█".repeat(filled)}${"░".repeat(20 - filled)}]`;
			const summary = `${remaining.toFixed(0)}% left`;
			const reset = formatResetTimestamp(row.resetsAt);
			const full = `${bar} ${summary}${reset ? ` (resets ${reset})` : ""}`;
			usageRows.push({
				label: row.label,
				value: visibleWidth(full) <= valueWidth ? full : `${summary}${reset ? ` (resets ${reset})` : ""}`,
				details: row.details,
			});
		}
	}
	usageRows.push({ label: "Details", value: "https://chatgpt.com/codex/settings/usage" });
	addSection("Usage Limits", usageRows);

	const resetRows: Array<{ label: string; value: string; details?: string }> = [
		{ label: "Available", value: data.bankedResets === undefined ? "Unknown" : String(data.bankedResets) },
	];
	if (data.applicableResets !== undefined) resetRows.push({ label: "Usable now", value: String(data.applicableResets) });
	if (!data.resetDetailsAvailable) {
		resetRows.push({ label: "Details", value: "Unavailable" });
	} else {
		data.resetCredits.forEach((credit, index) => {
			resetRows.push({
				label: `Reset ${index + 1}`,
				value: `${credit.title} · ${credit.expires}`,
				details: credit.description,
			});
		});
	}
	addSection("Banked Resets", resetRows);

	return lines.map((line) => truncateToWidth(` ${line}`, width, ""));
}

function resetCreditExpiry(expiresAt?: string | null): string {
	if (!expiresAt) return "Does not expire";
	const date = new Date(expiresAt);
	if (Number.isNaN(date.getTime())) return "Expiration unavailable";
	return `Expires ${date.toLocaleString([], {
		year: "numeric",
		month: "short",
		day: "numeric",
		hour: "2-digit",
		minute: "2-digit",
	})}`;
}

function availableResetCredits(details: ResetCreditsDetails | undefined): ResetCredit[] {
	if (!details) return [];
	const count = Math.max(0, Math.floor(details.available_count || 0));
	return (details.credits || [])
		.filter((credit) => (credit.status || "available").toLowerCase() === "available")
		.sort((left, right) => {
			const leftTime = left.expires_at ? Date.parse(left.expires_at) : Number.POSITIVE_INFINITY;
			const rightTime = right.expires_at ? Date.parse(right.expires_at) : Number.POSITIVE_INFINITY;
			return leftTime - rightTime;
		})
		.slice(0, count);
}

function resetCreditTitle(credit: ResetCredit): string {
	return credit.title?.trim() || "Full reset";
}

function resetCreditDescription(credit: ResetCredit): string {
	return credit.description?.trim() || "Reset your current usage limits.";
}

function formatExpiry(expires?: number): string {
	if (!expires) return "expiry unknown";
	const relativeMs = expires - Date.now();
	const absolute = new Date(expires).toLocaleString();
	if (relativeMs <= 0) return `expired ${absolute}`;
	const minutes = Math.max(1, Math.round(relativeMs / 60_000));
	return `expires ${absolute} (in ${minutes}m)`;
}

export default function codexUsageExtension(pi: ExtensionAPI) {

	async function requireCodexOAuth(ctx: ExtensionCommandContext): Promise<CodexRequestAuth | undefined> {
		if (ctx.model?.provider !== PROVIDER_ID) {
			ctx.ui.notify("This command requires an active OpenAI Codex model", "warning");
			return undefined;
		}
		if (!ctx.modelRegistry.isUsingOAuth(ctx.model)) {
			ctx.ui.notify("This command requires an OpenAI Codex OAuth login in Pi", "warning");
			return undefined;
		}
		try {
			return await resolveCodexOAuth(ctx);
		} catch (error) {
			ctx.ui.notify(error instanceof Error ? error.message : String(error), "warning");
			return undefined;
		}
	}

	type LoadedUsage = {
		usage?: UsagePayload;
		details?: ResetCreditsDetails;
		usageError?: string;
		detailsError?: string;
	};

	async function loadUsage(auth: CodexRequestAuth): Promise<LoadedUsage> {
		const [usageResult, detailsResult] = await Promise.allSettled([
			fetchCodexUsageWithAuth(auth),
			fetchResetCredits(auth),
		]);
		return {
			usage: usageResult.status === "fulfilled" ? usageResult.value : undefined,
			details: detailsResult.status === "fulfilled" ? detailsResult.value : undefined,
			usageError: usageResult.status === "rejected"
				? usageResult.reason instanceof Error ? usageResult.reason.message : String(usageResult.reason)
				: undefined,
			detailsError: detailsResult.status === "rejected"
				? detailsResult.reason instanceof Error ? detailsResult.reason.message : String(detailsResult.reason)
				: undefined,
		};
	}

	function appendUsage(loaded: LoadedUsage): void {
		const usage = loaded.usage;
		const details = loaded.details;
		const plan = planDisplayName(usage?.plan_type);
		const account = usage?.email && plan ? `${usage.email} (${plan})` : usage?.email || plan || "ChatGPT";
		const limits = usage ? composeLimitRows(usage) : [];
		const summary = usage?.rate_limit_reset_credits;
		const bankedResets = typeof summary?.available_count === "number"
			? Math.max(0, Math.floor(summary.available_count))
			: typeof details?.available_count === "number"
				? Math.max(0, Math.floor(details.available_count))
				: undefined;
		const applicableResets = typeof summary?.applicable_available_count === "number"
			? Math.max(0, Math.floor(summary.applicable_available_count))
			: undefined;
		pi.appendEntry<CodexUsageEntryData>("codex-usage", {
			account,
			limits,
			limitsAvailable: !!usage && limits.length > 0,
			bankedResets,
			applicableResets,
			resetCredits: availableResetCredits(details).map((credit) => ({
				title: resetCreditTitle(credit),
				expires: resetCreditExpiry(credit.expires_at),
				description: resetCreditDescription(credit),
			})),
			resetDetailsAvailable: !!details,
		});
	}

	async function redeemReset(
		ctx: ExtensionCommandContext,
		auth: CodexRequestAuth,
		loaded: LoadedUsage,
	): Promise<void> {
		const credits = availableResetCredits(loaded.details);
		const summaryCount = loaded.usage?.rate_limit_reset_credits?.available_count;
		const availableCount = typeof summaryCount === "number"
			? Math.max(0, Math.floor(summaryCount))
			: typeof loaded.details?.available_count === "number"
				? Math.max(0, Math.floor(loaded.details.available_count))
				: undefined;
		if (availableCount === undefined) {
			ctx.ui.notify("Could not load banked usage-limit resets; please try again", "warning");
			appendUsage(loaded);
			return;
		}
		if (availableCount === 0) {
			ctx.ui.notify("No banked usage-limit resets are available", "info");
			appendUsage(loaded);
			return;
		}

		let selected: ResetCredit | undefined;
		if (credits.length > 0) {
			const labels = credits.map((credit) => `${resetCreditTitle(credit)} — ${resetCreditExpiry(credit.expires_at)}`);
			const choice = await ctx.ui.select("Choose a banked usage-limit reset", labels);
			if (!choice) return;
			selected = credits[labels.indexOf(choice)];
			if (!selected) return;
		}

		const title = selected ? resetCreditTitle(selected) : "Full reset";
		const expiry = selected ? resetCreditExpiry(selected.expires_at) : "Specific reset details unavailable";
		const description = selected ? resetCreditDescription(selected) : "Reset your current usage limits.";
		const applicable = loaded.usage?.rate_limit_reset_credits?.applicable_available_count;
		const eligibility = applicable === 0
			? "\n\nYour current limits do not appear to need a reset; the service may leave it banked."
			: "";
		const confirmed = await ctx.ui.confirm(
			"Redeem banked reset?",
			`${title}\n${expiry}\n\n${description}${eligibility}\n\nThis action cannot be undone.`,
		);
		if (!confirmed) return;

		const currentAuth = await requireCodexOAuth(ctx);
		if (!currentAuth) return;
		if (currentAuth.accountId !== auth.accountId) {
			ctx.ui.notify("The active Codex account changed; reset was not redeemed", "warning");
			return;
		}

		let result: ConsumeResetResponse;
		try {
			result = await consumeResetCredit(currentAuth, randomUUID(), selected?.id);
		} catch (error) {
			ctx.ui.notify(error instanceof Error ? error.message : String(error), "error");
			return;
		}

		switch (result.code) {
			case "reset":
				ctx.ui.notify(`Usage reset${result.windows_reset ? ` (${result.windows_reset} limits reset)` : ""}`, "info");
				break;
			case "already_redeemed":
				ctx.ui.notify("That reset was already redeemed", "info");
				break;
			case "nothing_to_reset":
				ctx.ui.notify("Your usage does not need a reset right now; the reset remains banked", "info");
				break;
			case "no_credit":
				ctx.ui.notify("That banked reset is no longer available", "warning");
				break;
			default:
				ctx.ui.notify(`Unexpected reset response: ${result.code || "unknown"}`, "error");
		}

		const refreshed = await loadUsage(currentAuth);
		appendUsage(refreshed);
	}

	pi.registerEntryRenderer<CodexStatusEntryData>("codex-status", (entry, _options, theme) => {
		const data = entry.data;
		return {
			render: (width) => renderCodexStatus(data, width, theme),
			invalidate: () => {},
		};
	});

	pi.registerEntryRenderer<CodexUsageEntryData>("codex-usage", (entry, _options, theme) => {
		const data = entry.data;
		return {
			render: (width) => renderCodexUsage(data, width, theme),
			invalidate: () => {},
		};
	});

	pi.registerCommand("status", {
		description: "Show Codex account, context, credits, resets, and live usage limits",
		handler: async (_args, ctx) => {
			await ctx.waitForIdle();
			const auth = await requireCodexOAuth(ctx);
			if (!auth) return;

			let usage: UsagePayload | undefined;
			try {
				usage = await fetchCodexUsageWithAuth(auth);
			} catch {
				// Keep the status useful when the live limits endpoint is unavailable.
			}

			const credential = currentPiCredential();
			const context = ctx.getContextUsage();
			const contextWindow = Math.max(1, context?.contextWindow || ctx.model.contextWindow);
			const contextTokens = Math.max(0, context?.tokens || 0);
			const plan = planDisplayName(usage?.plan_type);
			// The account response is authorized by Pi's OAuth bearer token; do not
			// fall back to a Codex CLI auth file when displaying identity.
			const email = usage?.email;
			const account = email && plan ? `${email} (${plan})` : email || plan || "ChatGPT";
			const limits = usage ? composeLimitRows(usage) : [];

			pi.appendEntry<CodexStatusEntryData>("codex-status", {
				provider: ctx.model.provider,
				model: ctx.model.id,
				thinking: pi.getThinkingLevel(),
				directory: formatDirectory(ctx.cwd),
				account,
				authExpires: credential?.expires,
				threadName: pi.getSessionName(),
				session: ctx.sessionManager.getSessionId(),
				contextTokens,
				contextWindow,
				limits,
				limitsAvailable: limits.length > 0,
			});
		},
	});

	pi.registerCommand("usage", {
		description: "Show Codex usage or redeem a banked usage-limit reset",
		getArgumentCompletions: (prefix) => {
			const options = [
				{ value: "show", label: "show", description: "Show live usage and banked resets" },
				{ value: "reset", label: "reset", description: "Redeem a banked usage-limit reset" },
			];
			const filtered = options.filter((option) => option.value.startsWith(prefix.trim()));
			return filtered.length ? filtered : null;
		},
		handler: async (args, ctx) => {
			await ctx.waitForIdle();
			const auth = await requireCodexOAuth(ctx);
			if (!auth) return;
			const loaded = await loadUsage(auth);

			const action = args.trim().toLowerCase();
			if (action === "show") {
				appendUsage(loaded);
				return;
			}
			if (action === "reset") {
				await redeemReset(ctx, auth, loaded);
				return;
			}
			if (action) {
				ctx.ui.notify(`Unknown usage action "${action}"; use show or reset`, "error");
				return;
			}

			const summaryCount = loaded.usage?.rate_limit_reset_credits?.available_count;
			const availableCount = typeof summaryCount === "number"
				? Math.max(0, Math.floor(summaryCount))
				: typeof loaded.details?.available_count === "number"
					? Math.max(0, Math.floor(loaded.details.available_count))
					: undefined;
			const showChoice = "Show account usage";
			const resetChoice = availableCount === undefined
				? "Check banked usage-limit resets"
				: `Redeem banked usage-limit reset (${availableCount} available)`;
			const options = availableCount === 0 ? [showChoice] : [showChoice, resetChoice];
			const choice = await ctx.ui.select("Usage", options);
			if (choice === showChoice) appendUsage(loaded);
			else if (choice === resetChoice) await redeemReset(ctx, auth, loaded);
		},
	});

}
