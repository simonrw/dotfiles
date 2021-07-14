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
    use { 'vim-test/vim-test', cmd = {'TestSuite', 'TestFile', 'TestLast', 'TestNearest'} }
    use 'junegunn/vim-easy-align'
    use { 'junegunn/goyo.vim', cmd = {'Goyo'} }
    use 'shumphrey/fugitive-gitlab.vim'
    use { 'iamcco/markdown-preview.nvim', run = 'cd app && yarn install', ft = {'markdown'} }
    use 'airblade/vim-gitgutter' 

    -- language plugins
    use { 'cespare/vim-toml', ft = {'toml'} }
    use { 'jeetsukumaran/vim-pythonsense', ft = {'python'} }
    use { 'evanleck/vim-svelte', ft = {'svelte'} }
    use { 'ElmCast/elm-vim', ft = {'elm'} }
    use { 'leafgarland/typescript-vim', ft = {'typescript'} }
    use { 'hashivim/vim-terraform', ft = {'terraform'} }
    use { 'ziglang/zig.vim', ft = {'zig'} }
    use { 'jparise/vim-graphql', ft = {'graphql'} }
    use { 'jjo/vim-cue', ft = {'cue'} }
    use { 'vmchale/dhall-vim', ft = {'dhall'} }
    use { 'pest-parser/pest.vim', ft = {'pest'} }

    local completion_framework = vim.api.nvim_get_var("completion_framework")
    if completion_framework == "coc" then
        use { 'neoclide/coc.nvim', branch = 'release' }
        use { 'rodrigore/coc-tailwind-intellisense', run = 'npm install' }
    elseif completion_framework == 'ale' then
        use 'dense-analysis/ale'
        use { 'fatih/vim-go', ft = {'go'} }
    elseif completion_framework == 'nvim' then
        use 'neovim/nvim-lspconfig'
        use 'hrsh7th/nvim-compe'
        use 'RishabhRD/popfix'
        use 'RishabhRD/nvim-lsputils'
        use 'nvim-lua/lsp-status.nvim'
        use { 'psf/black', branch = 'stable', ft = {'python'} }
    end

    if vim.api.nvim_get_var("include_treesitter") == 1 then
        use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
    end
end,
config = {}})

