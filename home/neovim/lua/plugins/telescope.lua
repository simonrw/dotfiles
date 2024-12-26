return {
{
        'nvim-telescope/telescope.nvim',
        tag = '0.1.8',
        dependencies = {
            'nvim-lua/plenary.nvim',
            { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
        },
        config = function()
            require("telescope").setup({
                defaults = {
                    layout_config = {prompt_position = "top"},
                    layout_strategy = "horizontal",
                    sorting_strategy = "ascending"
                },
                extensions = {
                    fzf = {
                        case_mode = "smart_case",
                        fuzzy = true,
                        override_file_sorter = true,
                        override_generic_sorter = true
                    }
                },
                pickers = {
                    buffers = {disable_devicons = true},
                    current_buffer_fuzzy_find = {disable_devicons = true},
                    diagnostics = {disable_devicons = true},
                    find_files = {disable_devicons = true},
                    git_files = {disable_devicons = true},
                    live_grep = {disable_devicons = true},
                    lsp_definitions = {disable_devicons = true},
                    lsp_dynamic_workspace_symbols = {disable_devicons = true},
                    lsp_references = {disable_devicons = true}
                }
            })

            -- set up keymaps
            local __binds = {
                {
                    action = "<cmd>Telescope current_buffer_fuzzy_find<cr>",
                    key = "<leader>/",
                    mode = "n"
                },
                {
                    action = "<cmd>Telescope live_grep<cr>",
                    key = "<leader><space>",
                    mode = "n"
                },
                {
                    action = "<cmd>Telescope find_files<cr>",
                    key = "<leader>F",
                    mode = "n"
                },
                {
                    action = "<cmd>Telescope diagnostics<cr>",
                    key = "<leader>d",
                    mode = "n"
                },
                {
                    action = "<cmd>Telescope git_files<cr>",
                    key = "<leader>f",
                    mode = "n"
                }, {
                    action = "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>",
                    key = "<leader>s",
                    mode = "n"
                },
                {action = "<cmd>Telescope buffers<cr>", key = "gb", mode = "n"},
                {
                    action = "<cmd>Telescope lsp_definitions<cr>",
                    key = "gd",
                    mode = "n"
                },
                {
                    action = "<cmd>Telescope lsp_references<cr>",
                    key = "gr",
                    mode = "n"
                }
            }
            for i, map in ipairs(__binds) do
                vim.keymap.set(map.mode, map.key, map.action, map.options)
            end
        end
    }
}
