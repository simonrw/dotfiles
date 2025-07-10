return {
    {
        "mason-org/mason-lspconfig.nvim",
        dependencies = {
            { "mason-org/mason.nvim", opts = {} },
            "neovim/nvim-lspconfig",
        },
        opts = {
            ensure_installed = {
                -- Don't include rust_analyzer as it's usually installed by rustup
                "basedpyright",
                "lua_ls",
            },
        },
    }
}
