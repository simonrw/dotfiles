-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ 'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path })
    execute('packadd packer.nvim')
end

require('packer').startup({
    function(use)
        use 'wbthomason/packer.nvim'
        use 'christoomey/vim-tmux-runner'
        use 'christoomey/vim-conflicted'
        use 'projekt0n/github-nvim-theme'
        use 'shaunsingh/solarized.nvim'
        use { "catppuccin/nvim", as = "catppuccin" }

        -- language plugins
        use 'lepture/vim-velocity'
        use 'DingDean/wgsl.vim'
        use 'tweekmonster/django-plus.vim'
        use 'terrastruct/d2-vim'

        -- load telescope in here so we get the latest and greatest
        use {
            'nvim-telescope/telescope.nvim',
            cmd = "Telescope",
            setup = function()
                local mappings = require('mappings')

                mappings.nnoremap('<leader>f', [[<cmd>lua require('telescope.builtin').git_files()<Cr>]])
                mappings.nnoremap('<leader>F', [[<cmd>lua require('telescope.builtin').find_files()<Cr>]])
                mappings.nnoremap('gb', [[<cmd>lua require('telescope.builtin').buffers()<Cr>]])
                mappings.nnoremap('<leader><space>', [[<cmd>lua require('telescope.builtin').live_grep()<Cr>]])
                mappings.nnoremap('<leader>/', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<Cr>]])

                require("telescope").setup({
                    defaults = {
                        layout_strategy = 'horizontal',
                        layout_config = {
                            prompt_position = 'top',
                        },
                        sorting_strategy = 'ascending',
                    },
                    pickers = {
                        git_files = {
                            disable_devicons = true,
                        },
                        find_files = {
                            disable_devicons = true,
                        },
                    },
                    extensions = {
                        fzf = {
                            fuzzy = true,
                            override_generic_sorter = true,
                            override_file_sorter = true,
                            case_mode = "smart_case",
                        },
                    }
                })

                require("telescope").load_extension("fzf")
            end,
        }

        use 'averms/black-nvim'
        use 'jaredgorski/fogbell.vim'
        use 'lewis6991/gitsigns.nvim'

        use {
            'nvim-treesitter/nvim-treesitter-context',
            config = function()
                require('treesitter-context').setup({
                    max_lines = 5,
                })
            end,
        }

        use {
            'VonHeikemen/lsp-zero.nvim',
            branch = "v2.x",
            requires = {
                -- LSP Support
                { 'neovim/nvim-lspconfig' },
                { 'williamboman/mason.nvim' },
                { 'williamboman/mason-lspconfig.nvim' },

                -- Autocompletion
                { 'hrsh7th/nvim-cmp' },
                { 'hrsh7th/cmp-buffer' },
                { 'hrsh7th/cmp-path' },
                { 'saadparwaiz1/cmp_luasnip' },
                { 'hrsh7th/cmp-nvim-lsp' },
                { 'hrsh7th/cmp-nvim-lua' },

                -- Snippets
                { 'L3MON4D3/LuaSnip' },
            }
        }

        use {
            "folke/trouble.nvim",
            config = function()
                require("trouble").setup {
                    icons = false,
                    use_diagnostic_signs = false,
                    auto_preview = false,
                }

                vim.keymap.set("n", "yot", function() require('trouble').toggle() end, { remap = false })
            end
        }
    end,
    config = {}
})
