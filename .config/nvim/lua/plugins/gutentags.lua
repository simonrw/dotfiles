return {
    {
        "ludovicchabant/vim-gutentags",
        config = function()
            vim.g.gutentags_ctags_tagfile = ".tags"
            vim.g.gutentags_ctags_exclude = {".venv", "node_modules", "target"}
            vim.keymap.set("n", "gd", "<c-]>", { silent = true, noremap = true, desc = "Go to tag definitioc" })
        end,
    },
}
