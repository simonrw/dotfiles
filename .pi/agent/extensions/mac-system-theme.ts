/**
 * Syncs pi theme with macOS system appearance (dark/light mode).
 *
 * Usage:
 *   pi -e examples/extensions/mac-system-theme.ts
 */

import { exec } from "node:child_process";
import { promisify } from "node:util";
import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";

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

	pi.on("session_start", async (_event, ctx) => {
		let currentTheme = (await isDarkMode()) ? "dark" : "light";
		const themes = {
			dark: ctx.ui.getTheme("dark"),
			light: ctx.ui.getTheme("light"),
		};
		const setTheme = (name: keyof typeof themes) => {
			const theme = themes[name];
			if (theme) ctx.ui.setTheme(theme);
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

	pi.on("session_shutdown", () => {
		if (intervalId) {
			clearInterval(intervalId);
			intervalId = null;
		}
	});
}
