return {
    {
        "projekt0n/github-nvim-theme",
        priority = 1000,
        lazy = false,
        enabled = not vim.g.is_dark_theme,
        config = function()
            require("github-theme").setup({})
            vim.cmd.colorscheme "github_light"
        end,
    }
}
