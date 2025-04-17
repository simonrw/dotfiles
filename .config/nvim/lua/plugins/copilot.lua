return {
    {
        'github/copilot.vim',
        config = function()
            -- Remap completion keys
            vim.g.copilot_no_tab_map = true
            -- control + w (accept word)
            -- vim.keymap.set({ "i" }, "<C-w>", "<Plug>(copilot-accept-word)", { silent = true, desc = "Toggle CodeCompanion Chat" })
            -- control + e (accept line)
            vim.keymap.set({ "i" }, "<C-e>", "<Plug>(copilot-accept-line)", { silent = true, desc = "Toggle CodeCompanion Chat" })
        end
    }
}
