-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
    execute('packadd packer.nvim')
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
    use 'junegunn/vim-easy-align'
    use 'shumphrey/fugitive-gitlab.vim'
    use { 'iamcco/markdown-preview.nvim', run = 'cd app && yarn install' }
    use 'airblade/vim-gitgutter'
    use { 'kana/vim-textobj-indent', requires = { 'kana/vim-textobj-user' } }
    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} }
    }
    use 'TC72/telescope-tele-tabby.nvim'
    use {'nvim-telescope/telescope-ui-select.nvim' }
    use 'ludovicchabant/vim-gutentags'
    use 'editorconfig/editorconfig-vim'
    use 'nvim-lualine/lualine.nvim'
    use {
        "folke/zen-mode.nvim",
        config = function()
            require("zen-mode").setup {}
        end
    }

    -- language plugins
    use 'cespare/vim-toml'
    use 'evanleck/vim-svelte'
    use 'hashivim/vim-terraform'
    use 'lepture/vim-velocity'
    use 'NoahTheDuke/vim-just'
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
        use 'psf/black'
        use "williamboman/mason.nvim" 
        use 'williamboman/mason-lspconfig.nvim'
        use 'nvim-lua/lsp-status.nvim'
        use "lukas-reineke/lsp-format.nvim"

        -- debugging
        use { 'rcarriga/nvim-dap-ui', requires = {'mfussenegger/nvim-dap'} }
        use 'leoluz/nvim-dap-go'
        use 'mfussenegger/nvim-dap-python'

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
        use 'nvim-treesitter/nvim-treesitter-textobjects'
    end

    -- for testing
    use 'nvim-lua/plenary.nvim'

    -- my plugins
    use 'mindriot101/search-in-scope.vim'
end,
config = {}})

require('search_in_scope').setup({
    bind = '<leader>S',
    indent_filetypes = {'lua'},
})
