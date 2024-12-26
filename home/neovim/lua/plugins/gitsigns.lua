return {
    {
        "lewis6991/gitsigns.nvim",
        config = function()
            require('gitsigns').setup()

            -- keymaps
            local __binds = {
                {
                    action = function()
                        if vim.wo.diff then return "]c" end
                        vim.schedule(function()
                            require("gitsigns").next_hunk()
                        end)
                        return "<Ignore>"
                    end,
                    key = "]c",
                    mode = "n",
                    options = {expr = true, silent = true}
                }, {
                    action = function()
                        if vim.wo.diff then return "]c" end
                        vim.schedule(function()
                            require("gitsigns").prev_hunk()
                        end)
                        return "<Ignore>"
                    end,
                    key = "[c",
                    mode = "n",
                    options = {expr = true, silent = true}
                }, {
                    action = function()
                        require("gitsigns").diffthis("~")
                    end,
                    key = "<leader>hD",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = require("gitsigns").reset_buffer,
                    key = "<leader>hR",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = require("gitsigns").stage_buffer,
                    key = "<leader>hS",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = function()
                        require("gitsigns").blame_line({full = true})
                    end,
                    key = "<leader>hb",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = require("gitsigns").diffthis,
                    key = "<leader>hd",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = require("gitsigns").preview_hunk,
                    key = "<leader>hp",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = require("gitsigns").reset_hunk,
                    key = "<leader>hr",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = require("gitsigns").stage_hunk,
                    key = "<leader>hs",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = require("gitsigns").undo_stage_hunk,
                    key = "<leader>hu",
                    mode = "n",
                    options = {silent = true}
                }, {
                    action = function()
                        require("gitsigns").reset_hunk({
                            vim.fn.line("."), vim.fn.line("v")
                        })
                    end,
                    key = "<leader>hr",
                    mode = "v",
                    options = {silent = true}
                }, {
                    action = function()
                        require("gitsigns").stage_hunk({
                            vim.fn.line("."), vim.fn.line("v")
                        })
                    end,
                    key = "<leader>hs",
                    mode = "v",
                    options = {silent = true}
                }
            }
            for _, map in ipairs(__binds) do
                vim.keymap.set(map.mode, map.key, map.action, map.options)
            end
        end
    }
}
