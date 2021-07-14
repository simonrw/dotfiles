return require('packer').startup(function()
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
    use 'tpope/vim-dispatch'
    use 'tpope/vim-obsession'
    use 'christoomey/vim-tmux-runner'
    use 'christoomey/vim-conflicted'
    use 'vim-test/vim-test'
    use 'junegunn/vim-easy-align'
    use 'junegunn/goyo.vim'
    use 'chriskempson/base16-vim'
    use 'shumphrey/fugitive-gitlab.vim'
    use 'NLKNguyen/papercolor-theme'
    use 'direnv/direnv.vim'
    use { 'iamcco/markdown-preview.nvim', run = ':mkdp#util#install()', ft = {'markdown'} }

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
    use 'airblade/vim-gitgutter'

    if vim.api.nvim_get_var("completion_framework") == "coc" then
        use { 'neoclide/coc.nvim', branch = 'release' }
        use { 'rodrigore/coc-tailwind-intellisense', run = 'npm install' }
    elseif vim.api.nvim_get_var("completion_framework") == 'ale' then
        use 'dense-analysis/ale'
        use 'fatih/vim-go'
    elseif vim.api.nvim_get_var("completion_framework") == 'nvim' then
        use 'neovim/nvim-lspconfig'
        use 'hrsh7th/nvim-compe'
        use 'RishabhRD/popfix'
        use 'RishabhRD/nvim-lsputils'
        use 'nvim-lua/lsp-status.nvim'
        use { 'psf/black', branch = 'stable' }
    end

    if vim.api.nvim_get_var("include_treesitter") == 1 then
        use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
    end
end)

