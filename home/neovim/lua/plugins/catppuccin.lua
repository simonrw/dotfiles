return {

    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        config = function()
            vim.cmd([[
                set background=dark
                colorscheme catppuccin-macchiato
            ]])
            vim.cmd.highlight({"TreesitterContextBottom", "gui=none"})
        end
    },
}
