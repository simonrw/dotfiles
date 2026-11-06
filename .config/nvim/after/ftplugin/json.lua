vim.keymap.set('n', '<leader>y', function()
    vim.cmd("%!jq .")
end, { noremap = true, silent = true, buf = 0 })
