-- override the format command to pipe through jq
vim.keymap.set('n', '<leader>y', function()
    vim.cmd("%!jq .")
end, { noremap = true, silent = true })
