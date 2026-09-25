/**
 * Syncs pi theme with macOS appearance until /theme-toggle is used.
 * Manual changes last until /reload or restart and do not change saved settings.
 */

import { exec } from "node:child_process";
import { promisify } from "node:util";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const execAsync = promisify(exec);

async function isDarkMode(): Promise<boolean> {
	try {
		const { stdout } = await execAsync(
			"osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'",
		);
		return stdout.trim() === "true";
	} catch {
		return false;
	}
}

export default function (pi: ExtensionAPI) {
	let intervalId: ReturnType<typeof setInterval> | null = null;
	let syncStopped = false;

	function stopSync() {
		syncStopped = true;
		if (intervalId) {
			clearInterval(intervalId);
			intervalId = null;
		}
	}

	pi.registerCommand("theme-toggle", {
		description: "Toggle light/dark theme and pause automatic syncing until reload",
		handler: async (_args, ctx) => {
			if (ctx.mode !== "tui") return;

			const name = ctx.ui.theme.name === "light" ? "dark" : "light";
			const theme = ctx.ui.getTheme(name);
			if (!theme) {
				ctx.ui.notify(`Theme not found: ${name}`, "error");
				return;
			}

			// Use a Theme object to leave saved settings unchanged.
			const result = ctx.ui.setTheme(theme);
			if (!result.success) {
				ctx.ui.notify(result.error ?? `Could not set theme: ${name}`, "error");
				return;
			}

			stopSync();
			ctx.ui.notify(`Theme: ${name}. Automatic syncing paused until /reload.`, "info");
		},
	});

	pi.on("session_start", async (_event, ctx) => {
		if (ctx.mode !== "tui" || syncStopped) return;

		let currentTheme = (await isDarkMode()) ? "dark" : "light";
		if (syncStopped) return;
		const themes = {
			dark: ctx.ui.getTheme("dark"),
			light: ctx.ui.getTheme("light"),
		};
		const setTheme = (name: keyof typeof themes) => {
			const theme = themes[name];
			// A macOS appearance check may finish after a manual toggle or shutdown.
			if (theme && !syncStopped) ctx.ui.setTheme(theme);
		};
		setTheme(currentTheme);

		intervalId = setInterval(async () => {
			const newTheme = (await isDarkMode()) ? "dark" : "light";
			if (newTheme !== currentTheme) {
				currentTheme = newTheme;
				setTheme(currentTheme);
			}
		}, 2000);
	});

	pi.on("session_shutdown", stopSync);
}
