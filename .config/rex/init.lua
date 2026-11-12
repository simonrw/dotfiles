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

local function must(result, err)
	if not result then error(err, 0) end
	return result
end

local function zoomed(block_id)
	for _, window in ipairs(must(rex.session.view {}).windows) do
		if window.zoomed_block_id == block_id then return true end
	end
	return false
end

-- Moves `block_id` into the layout slot of `slot_id`, leaving `slot_id` detached but running.
local function take_slot(block_id, slot_id)
	local zoom = zoomed(slot_id)
	must(rex.session.move_block { block_id = block_id, anchor_block_id = slot_id })
	must(rex.session.detach_block { block_id = slot_id })
	if zoom then must(rex.session.zoom_block { block_id = block_id, zoom = true }) end
	must(rex.session.focus_block { block_id = block_id })
end

local copy_mode_label = "Copy mode: "

rex.action {
	name = "copy_mode",
	title = "Copy mode",
	description = "Select terminal scrollback with Vim keys and copy it",
	run = function(ctx)
		local source = must(rex.session.describe_block { block_id = ctx.block_id }).block
		if source.label:sub(1, #copy_mode_label) == copy_mode_label then return end
		if source.creator_name ~= "com.superlogical.terminal" then
			error("Copy mode requires a terminal pane", 0)
		end

		local cli = copy_mode_executable("rex")
		local viewer = must(rex.session.new_block {
			flavor = "com.superlogical.terminal.shell",
			label = copy_mode_label .. ctx.block_id,
			options = {
				shell = "none",
				command = {
					"/bin/sh", "-c",
					[["$@"; exec "$0" --autostart=false do copy_mode_restore --session "$REX_SESSION" --block "$REX_BLOCK"]],
					cli, copy_mode_executable("rex-copy-mode"),
					"-rex", cli, "-session", ctx.session_id, "-block", ctx.block_id,
				},
				exit = { on_completion = true },
			},
		}).block_id

		take_slot(viewer, ctx.block_id)
		return { block_ids = { viewer } }
	end,
}

rex.action {
	name = "copy_mode_restore",
	title = "Copy mode: restore pane",
	description = "Put a copy mode viewer's source terminal back in its place",
	run = function(ctx)
		local viewer = must(rex.session.describe_block { block_id = ctx.block_id }).block
		if viewer.label:sub(1, #copy_mode_label) ~= copy_mode_label then return end

		take_slot(viewer.label:sub(#copy_mode_label + 1), ctx.block_id)
		must(rex.call("block.close", { session_id = ctx.session_id, block_id = ctx.block_id }))
	end,
}

rex.bind(prefix .. "[", "copy_mode")
