return {
    {
        "CopilotC-Nvim/CopilotChat.nvim",
        dependencies = {
            {
                'github/copilot.vim',
                config = function()
                    -- Remap completion keys
                    vim.g.copilot_no_tab_map = true
                    -- control + w (accept word)
                    -- vim.keymap.set({ "i" }, "<C-w>", "<Plug>(copilot-accept-word)", { silent = true, desc = "Toggle CodeCompanion Chat" })
                    -- control + e (accept line)
                    vim.keymap.set({ "i" }, "<C-e>", "<Plug>(copilot-accept-line)",
                        { silent = true, desc = "Toggle CodeCompanion Chat" })
                end
            },
            { "nvim-lua/plenary.nvim" }, -- for curl, log and async functions
        },
        build = "make tiktoken",        -- Only on MacOS or Linux
        opts = {
            -- See Configuration section for options
        },
        -- See Commands section for default commands if you want to lazy load on them
    },
}
