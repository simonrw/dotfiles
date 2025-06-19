return {
    {
        "projekt0n/github-nvim-theme",
        priority = 1000,
        lazy = false,
        enabled = not vim.g.is_dark_mode,
        config = function()
            require("github-theme").setup({})
            vim.cmd.colorscheme "github_light"

            vim.cmd.highlight({ "DiagnosticError", "guifg=Red" })
            vim.cmd.highlight({ "DiagnosticHint", "guifg=Orange" })
            vim.cmd.highlight({ "DiagnosticWarn", "guifg=Orange" })
            vim.cmd.highlight({ "DiagnosticInfo", "guifg=LightBlue" })
            vim.cmd.highlight({ "DiagnosticFloatingError", "guifg=Red" })
            vim.cmd.highlight({ "DiagnosticFloatingHint", "guifg=Orange" })
            vim.cmd.highlight({ "DiagnosticFloatingInfo", "guifg=LightBlue" })
            vim.cmd.highlight({ "DiagnosticFloatingWarn", "guifg=Orange" })
            vim.cmd.highlight({ "DiagnosticVirtualTextError", "guifg=Red" })
            vim.cmd.highlight({ "DiagnosticVirtualTextHint", "guifg=Orange" })
            vim.cmd.highlight({ "DiagnosticVirtualTextInfo", "guifg=LightBlue" })
            vim.cmd.highlight({ "DiagnosticVirtualTextWarn", "guifg=Orange" })
            vim.cmd.highlight({ "Comment", "guifg=#e69340" })
            vim.cmd.highlight({ "TreesitterContext", "guibg=#f0f0f0" })
        end,
    }
}
