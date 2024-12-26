return {
    {
        "aaronik/treewalker.nvim",
        opts = {
            highlight = true, -- Whether to briefly highlight the node after jumping to it
            highlight_duration = 250 -- How long should above highlight last (in ms)
        },
        config = function() 
            vim.api.nvim_set_keymap('n', '<C-n>', ':Treewalker Down<CR>', { noremap = true })
            vim.api.nvim_set_keymap('n', '<C-p>', ':Treewalker Up<CR>', { noremap = true })
            vim.api.nvim_set_keymap('n', '<C-e>', ':Treewalker Left<CR>', { noremap = true })
            vim.api.nvim_set_keymap('n', '<C-r>', ':Treewalker Right<CR>', { noremap = true })
        end
    }
}
