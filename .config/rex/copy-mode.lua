-- An isolated, in-memory snapshot viewer. No user config or clipboard provider is loaded.
vim.opt.shada = ""
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.undofile = false
vim.opt.modeline = false
vim.opt.clipboard = ""
vim.opt.mouse = ""
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.signcolumn = "no"
vim.opt.wrap = true
vim.opt.selection = "inclusive"
vim.opt.virtualedit = ""
vim.opt.laststatus = 2
vim.opt.statusline = " Copy mode | Space: select | Enter: copy | q/Esc: cancel %=%l/%L "
vim.opt.shortmess:append("I")
vim.opt.buftype = "nofile"
vim.opt.bufhidden = "wipe"

local function cancel()
	vim.cmd.quitall { bang = true }
end

vim.keymap.set({ "n", "x" }, "q", cancel)
vim.keymap.set({ "n", "x" }, "<Esc>", cancel)
vim.keymap.set("n", "<CR>", "<Nop>")

local function show_error(message)
	local lines = vim.split("Copy mode failed\n\n" .. message .. "\nPress q or Escape to close.", "\n", { plain = true })
	vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
	vim.opt.modifiable = false
end

local request = vim.json.encode {
	session_id = vim.env.REX_COPY_SOURCE_SESSION,
	block_id = vim.env.REX_COPY_SOURCE_BLOCK,
	args = { format = "text", trim = false, unwrap = true },
}
local ok, capture = pcall(function()
	return vim.system({ vim.env.REX_COPY_REX, "--autostart=false", "api", "call", "com.superlogical.terminal.format", request },
		{ text = true }):wait(10000)
end)
if not ok or capture.code ~= 0 then
	show_error(ok and capture.stderr or tostring(capture))
	return
end

local snapshot = vim.json.decode(capture.stdout).content
vim.api.nvim_buf_set_lines(0, 0, -1, false, vim.split(snapshot, "\n", { plain = true }))
vim.opt.modified = false
vim.opt.modifiable = false
vim.cmd.normal { args = { "G0" }, bang = true }

vim.keymap.set("n", "<Space>", "v")
vim.keymap.set("x", "<Space>", "<Nop>")
vim.keymap.set("x", "<CR>", function()
	-- Read the region directly so selecting a complete line doesn't add a newline.
	local mode = vim.fn.mode()
	local lines = vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."), { type = mode, exclusive = false })
	local text = table.concat(lines, "\n") .. (mode == "V" and "\n" or "")
	local copied, result = pcall(function()
		return vim.system({ "pbcopy" }, { stdin = text }):wait(10000)
	end)
	if not copied or result.code ~= 0 then
		vim.notify("Could not copy: " .. (copied and result.stderr or tostring(result)), vim.log.levels.ERROR)
		return
	end
	cancel()
end)
