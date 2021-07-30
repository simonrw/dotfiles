-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
    execute('packadd packer.nvim')
end

return require('packer').startup({function(use)
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
    use { 'iamcco/markdown-preview.nvim', run = 'cd app && yarn install', ft = {'markdown'} }

    -- language plugins
    use 'cespare/vim-toml'
    use 'jeetsukumaran/vim-pythonsense'
    use 'evanleck/vim-svelte'
    use 'ElmCast/elm-vim'
    use 'leafgarland/typescript-vim'
    use 'hashivim/vim-terraform'
    use 'ziglang/zig.vim'
    use 'jparise/vim-graphql'
    use 'jjo/vim-cue'
    use 'vmchale/dhall-vim'
    use 'pest-parser/pest.vim'
    use 'lepture/vim-velocity'

    if vim.g.completion_framework == "coc" then
        use { 'neoclide/coc.nvim', branch = 'release' }
    elseif vim.g.completion_framework == 'ale' then
        use 'dense-analysis/ale'
        use { 'fatih/vim-go', ft = {'go'} }
    elseif vim.g.completion_framework == 'nvim' then
        use 'neovim/nvim-lspconfig'
        use 'hrsh7th/nvim-compe'
        use 'RishabhRD/popfix'
        use 'RishabhRD/nvim-lsputils'
        use 'nvim-lua/lsp-status.nvim'
        use { 'psf/black', branch = 'stable', ft = {'python'} }
    end

    if vim.g.include_treesitter == 1 then
        use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
    end
end,
config = {}})

