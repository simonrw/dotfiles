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
    use 'tpope/vim-vinegar'
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
    use 'itchyny/lightline.vim'
    use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
    use { 'kana/vim-textobj-indent', requires = { 'kana/vim-textobj-user' } }
    use 'David-Kunz/treesitter-unit'

    -- language plugins
    use 'cespare/vim-toml'
    use 'evanleck/vim-svelte'
    use 'hashivim/vim-terraform'
    use 'lepture/vim-velocity'

    if vim.g.completion_framework == "coc" then
        use { 'neoclide/coc.nvim', branch = 'release' }
    elseif vim.g.completion_framework == 'ale' then
        use 'dense-analysis/ale'
        use 'fatih/vim-go'
    elseif vim.g.completion_framework == 'nvim' then
        use 'neovim/nvim-lspconfig'
        use 'nvim-lua/lsp_extensions.nvim'
        use 'nvim-lua/completion-nvim'
        use { 'psf/black', branch = 'stable' }
        use 'fatih/vim-go'
    end

    if vim.g.include_treesitter == 1 then
        use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
        use 'nvim-treesitter/playground'
    end

    -- for testing
    use 'nvim-lua/plenary.nvim'

    -- my plugins
    use 'mindriot101/search-in-scope.vim'
end,
config = {}})

-- package setups
require('gitsigns').setup()

require('search_in_scope').setup({
    bind = "<leader>S",
    indent_filetypes = {"lua"},
})
