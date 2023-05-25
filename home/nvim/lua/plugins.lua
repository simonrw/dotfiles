-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ 'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path })
    execute('packadd packer.nvim')
end

require('packer').startup({ function(use)
    use 'wbthomason/packer.nvim'
    use 'christoomey/vim-tmux-runner'
    use 'christoomey/vim-conflicted'
    use 'projekt0n/github-nvim-theme'

    -- language plugins
    use 'lepture/vim-velocity'
    use 'DingDean/wgsl.vim'
    use 'tweekmonster/django-plus.vim'
    use 'terrastruct/d2-vim'

    -- load telescope in here so we get the latest and greatest
    use 'nvim-telescope/telescope.nvim'
    use 'nvim-telescope/telescope-ui-select.nvim'

    use 'averms/black-nvim'

    use {
        "loctvl842/monokai-pro.nvim",
        config = function()
            require("monokai-pro").setup()
        end
    }

    use {
        'VonHeikemen/lsp-zero.nvim',
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
    config = {} })
