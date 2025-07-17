return {
    {
        "projekt0n/github-nvim-theme",
        priority = 1000,
        lazy = false,
        config = function()
            require("github-theme").setup({})
        end,
    }
}
