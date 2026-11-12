local prefix = "ctrl+s>"

rex.bind(prefix .. "s", "pane.send_key", { key = "ctrl+s" })
rex.bind(prefix .. "ctrl+z", function() return true end)

rex.bind(prefix .. "c", "client.tab.new")
rex.bind(prefix .. "n", "client.tab.next")
rex.bind(prefix .. "p", "client.tab.previous")
rex.bind(prefix .. "ctrl+n", "client.tab.next")
rex.bind(prefix .. "ctrl+p", "client.tab.previous")
rex.bind(prefix .. "9", "session.previous")
rex.bind(prefix .. "0", "session.next")

for index = 1, 8 do
	rex.bind(prefix .. index, "client.tab.goto", { index = index })
end

rex.bind(prefix .. "shift+\\", "pane.split", { direction = "right" })
rex.bind(prefix .. "-", "pane.split", { direction = "down" })
rex.bind(prefix .. "i", "pane.split", { direction = "auto" })
rex.bind(prefix .. "z", "pane.zoom")
rex.bind(prefix .. "x", "pane.close")
rex.bind(prefix .. "b", "pane.move_to_new_tab")

for key, direction in pairs({ h = "left", j = "down", k = "up", l = "right" }) do
	rex.bind(prefix .. key, "pane.focus", { direction = direction })
	-- Rex resizes by percentage, not tmux's five terminal cells.
	rex.bind(prefix .. "shift+" .. key, "pane.resize", { direction = direction, amount = 5 })
end

rex.bind(prefix .. "r", "client.config.reload")

rex.bind(prefix .. "f", "session.switch")

-- GUI-launched Rex does not inherit the interactive shell's PATH.
local copy_mode_path = os.getenv("HOME") .. "/.bin:" .. os.getenv("PATH")

local function copy_mode_executable(name)
	local result, err = rex.exec({ "which", name }, { timeout = 5, env = { PATH = copy_mode_path } })
	if not result then error(err, 0) end
	if not result.ok then error("Copy mode requires " .. name .. " on PATH", 0) end
	return result.stdout:gsub("%s+$", "")
end

rex.action {
	name = "copy_mode",
	title = "Copy mode",
	description = "Select terminal scrollback with Vim keys and copy it",
	run = function(ctx)
		local source, err = rex.session.describe_block { block_id = ctx.block_id }
		if not source then error(err, 0) end
		if source.block.label:match("^Copy mode: ") then return end
		if source.block.creator_name ~= "com.superlogical.terminal" then
			error("Copy mode requires a terminal pane", 0)
		end

		local label = "Copy mode: " .. ctx.block_id
		local blocks
		blocks, err = rex.session.list_blocks {}
		if not blocks then error(err, 0) end
		for _, block in ipairs(blocks.blocks) do
			if block.label == label and block.placement == "placed" then
				local focused
				focused, err = rex.session.focus_block { block_id = block.block_id }
				if not focused then error(err, 0) end
				return { block_ids = { block.block_id } }
			end
		end

		local viewer_bin = copy_mode_executable("rex-copy-mode")
		local cli = copy_mode_executable("rex")

		local viewer
		viewer, err = rex.session.new_layer {
			window_id = source.block.window_id,
			bounds = { 0, 0, 1, 1 },
			focus = true,
			layout = {
				block = {
					flavor = "com.superlogical.terminal.shell",
					label = label,
					options = {
						shell = "none",
						command = {
							"/usr/bin/env",
							"REX_COPY_SOURCE_SESSION=" .. ctx.session_id,
							"REX_COPY_SOURCE_BLOCK=" .. ctx.block_id,
							"REX_COPY_REX=" .. cli,
							-- Closing a floating layer can otherwise focus a sibling instead of the source.
							"/bin/sh", "-c",
							[["$@"; "$REX_COPY_REX" --autostart=false focus -s "$REX_COPY_SOURCE_SESSION" "$REX_COPY_SOURCE_BLOCK"; exec "$REX_COPY_REX" --autostart=false block close -s "$REX_SESSION" "$REX_BLOCK"]],
							"rex-copy-mode",
							viewer_bin, "-rex", cli, "-session", ctx.session_id, "-block", ctx.block_id,
						},
						exit = { on_completion = true },
					},
				},
			},
		}
		if not viewer then error(err, 0) end
		return viewer
	end,
}

rex.bind(prefix .. "[", "copy_mode")
