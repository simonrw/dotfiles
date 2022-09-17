vim.api.nvim_set_keymap('v', 'x', [[:lua require'treesitter-unit'.select()<Cr>]], { noremap = true })
vim.api.nvim_set_keymap('o', 'x', [[:<C-u>lua require'treesitter-unit'.select()<Cr>]], { noremap = true })
