return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            local configs = require("nvim-treesitter.configs")

            configs.setup({
                ensure_installed = {"lua", "vim", "rust", "python", "nix", "yaml", "hcl", "terraform"},
                sync_install = false,
                highlight = {enable = true},
                indent = {enable = false}
            })
        end
    },
}
