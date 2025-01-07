return {
    {
        'stevearc/oil.nvim',
        ---@module 'oil'
        ---@type oil.SetupOpts
        opts = {},
        -- Optional dependencies
        dependencies = {{"echasnovski/mini.icons", opts = {}}},
        -- dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if prefer nvim-web-devicons
        config = function()
            require("oil").setup({
                columns = {"icon"},
                keymaps = {["<C-h>"] = false},
                view_options = {show_hidden = true}
            })
            local binds = {
                {
                    action = "<Cmd>Oil<CR>",
                    key = "-",
                    mode = "n",
                    options = {noremap = true, silent = true, desc = "Show file browser (oil)"}
                }
            }
            for _, map in ipairs(binds) do
                vim.keymap.set(map.mode, map.key, map.action, map.options)
            end
        end
    }
}
