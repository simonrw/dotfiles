-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
    execute('packadd packer.nvim')
end

function telescope_setup()
    local mappings = require('mappings')

    mappings.nnoremap('<leader>f', [[<cmd>lua require('telescope.builtin').git_files({previewer = false})<Cr>]])
    mappings.nnoremap('<leader>F', [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<Cr>]])
    mappings.nnoremap('gb', [[<cmd>lua require('telescope.builtin').buffers()<Cr>]])
    mappings.nnoremap('gl', [[<cmd>lua require('telescope.builtin').live_grep()<Cr>]])
    mappings.nnoremap('<leader>gT', [[<cmd>lua require('telescope.builtin').tags()<Cr>]])
end

function telescope_config()
    local mappings = require('mappings')
    if vim.g.completion_framework == 'coc' then
        require('telescope').load_extension('coc')
        mappings.nnoremap('<leader>gt', [[<cmd>Telescope coc workspace_symbols<Cr>]])
    elseif vim.g.completion_framework == 'nvim' then
        mappings.nnoremap('<leader>gt', [[<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<Cr>]])
    end

    mappings.nnoremap('<leader>f', [[<cmd>lua require('telescope.builtin').git_files({previewer = false})<Cr>]])
    mappings.nnoremap('<leader>F', [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<Cr>]])
    mappings.nnoremap('gb', [[<cmd>lua require('telescope.builtin').buffers()<Cr>]])
    mappings.nnoremap('gl', [[<cmd>lua require('telescope.builtin').live_grep()<Cr>]])
    mappings.nnoremap('<leader>gT', [[<cmd>lua require('telescope.builtin').tags()<Cr>]])

    require("telescope").setup({
    extensions = {
        ["ui-select"] = {},
        ["tele_tabby"] = {},
    }
    })

    require("telescope").load_extension("ui-select")
    require("telescope").load_extension("tele_tabby")

    mappings.nnoremap('<leader>T', [[<cmd>lua require('telescope').extensions.tele_tabby.list()<Cr>]])
end

require('packer').startup({function(use)
    use 'wbthomason/packer.nvim'
    use 'lotabout/skim'
    use 'lotabout/skim.vim'
    use 'tpope/vim-surround'
    use 'tpope/vim-unimpaired'
    use 'tpope/vim-commentary'
    use 'tpope/vim-eunuch'
    use 'tpope/vim-fugitive'
    use 'tpope/vim-rhubarb'
    use 'tpope/vim-repeat'
    use 'christoomey/vim-tmux-runner'
    use 'christoomey/vim-conflicted'
    use 'vim-test/vim-test'
    use 'shumphrey/fugitive-gitlab.vim'
    use { 'iamcco/markdown-preview.nvim', run = 'cd app && yarn install' }
    use 'airblade/vim-gitgutter'
    use { 'kana/vim-textobj-indent', requires = { 'kana/vim-textobj-user' } }
    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        keys = { '<leader>f', },
        setup = telescope_setup,
        config = telescope_config,
    }
    use { 'TC72/telescope-tele-tabby.nvim', after = 'telescope.nvim' }
    use { 'nvim-telescope/telescope-ui-select.nvim', after = 'telescope.nvim' }
    use 'ludovicchabant/vim-gutentags'
    use 'nvim-lualine/lualine.nvim'

    -- language plugins
    use 'cespare/vim-toml'
    use 'hashivim/vim-terraform'
    use 'rust-lang/rust.vim'
    use 'tweekmonster/django-plus.vim'

    if vim.g.completion_framework == 'coc' then
        use { 'neoclide/coc.nvim', branch = 'release' }
        use 'fannheyward/telescope-coc.nvim'
    elseif vim.g.completion_framework == 'coq' then
        use { 'ms-jpq/coq_nvim', branch = 'coq' }
    elseif vim.g.completion_framework == 'ale' then
        use 'dense-analysis/ale'
    elseif vim.g.completion_framework == 'nvim' then
        use 'neovim/nvim-lspconfig'
        use 'nvim-lua/lsp_extensions.nvim'
        use "williamboman/mason.nvim" 
        use 'williamboman/mason-lspconfig.nvim'
        use {
            'nvim-lua/lsp-status.nvim',
            config = function()
                require('lsp-status').config({

                })
            end,
        }
        use "lukas-reineke/lsp-format.nvim"

        -- Completion
        use 'hrsh7th/cmp-nvim-lsp'
        use 'hrsh7th/cmp-buffer'
        use 'hrsh7th/nvim-cmp'
        use 'hrsh7th/cmp-vsnip'
        use 'hrsh7th/cmp-emoji'
        use 'hrsh7th/vim-vsnip'
        use 'onsails/lspkind-nvim'
    end

    if vim.g.include_treesitter == 1 then
        use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
        use 'romgrk/nvim-treesitter-context'
        use 'nvim-treesitter/playground'
    end

    -- my plugins
    use { 'mindriot101/search-in-scope.vim',
        config = function()
            require('search_in_scope').setup({
                bind = '<leader>S',
                indent_filetypes = {'lua'},
            })
        end,
    }
end,
config = {}})
