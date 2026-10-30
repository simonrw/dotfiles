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

local config_dir = (os.getenv("XDG_CONFIG_HOME") or os.getenv("HOME") .. "/.config") .. "/rex"
-- GUI-launched Rex does not inherit the interactive shell's PATH.
local copy_mode_path = os.getenv("HOME") .. "/.bin:" .. os.getenv("HOME") .. "/.local/share/bob/nvim-bin:" .. os.getenv("PATH")

local function copy_mode_executable(name)
	local result, err = rex.exec({ "which", name }, { timeout = 5, env = { PATH = copy_mode_path } })
	if not result then error(err, 0) end
	if not result.ok then error("Copy mode requires " .. name .. " on PATH", 0) end
	return result.stdout:gsub("%s+$", "")
end

rex.action {
	name = "copy_mode",
	title = "Copy mode",
	description = "Select terminal text with Vim keys, Space, and Enter",
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

		local view
		view, err = rex.session.view {}
		if not view then error(err, 0) end
		local function source_rect(node)
			if not node then return end
			if node.block_id == ctx.block_id then return node.rect end
			if node.split then
				return source_rect(node.split.before) or source_rect(node.split.after)
			end
		end
		local bounds
		for _, window in ipairs(view.windows) do
			if window.window_id == source.block.window_id then
				if window.zoomed_block_id == ctx.block_id then
					bounds = { 0, 0, 1, 1 }
				else
					for _, layer in ipairs(window.layers) do
						bounds = source_rect(layer.layout)
						if bounds then break end
					end
				end
			end
		end
		if not bounds then error("Copy mode could not locate the source pane", 0) end

		local nvim = copy_mode_executable("nvim")
		local cli = copy_mode_executable("rex")

		local viewer
		viewer, err = rex.session.new_layer {
			window_id = source.block.window_id,
			bounds = bounds,
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
							nvim, "--noplugin", "-n", "-i", "NONE", "-u", config_dir .. "/copy-mode.lua",
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
