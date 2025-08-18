local bufnr = vim.api.nvim_get_current_buf()

vim.keymap.set("n", "<leader>y", function()
    local filename = vim.fn.expand("%")
    vim.cmd.update()
    vim.system({ "ruff", "format", filename}, { text = true }):wait()
    vim.cmd.edit()
end, { silent = true, buffer = bufnr, desc = "Format current file" })
