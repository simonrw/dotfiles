return {
    {
        "allaman/emoji.nvim",
        version = "1.0.0", -- optionally pin to a tag
        ft = "markdown", -- adjust to your needs
        dependencies = {
            -- util for handling paths
            "nvim-lua/plenary.nvim",
            -- optional for telescope integration
            "nvim-telescope/telescope.nvim",
        },
        config = function(_, opts)
            require("emoji").setup(opts)
            -- optional for telescope integration
            local ts = require('telescope').load_extension 'emoji'
            vim.keymap.set('n', '<leader>se', ts.emoji, { desc = '[S]earch [E]moji' })
        end,
    }
}
