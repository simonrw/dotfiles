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
    use 'tpope/vim-surround'
    use 'tpope/vim-unimpaired'
    use 'tpope/vim-commentary'
    use 'tpope/vim-eunuch'
    use 'tpope/vim-fugitive'
    use 'tpope/vim-rhubarb'
    use 'tpope/vim-repeat'
    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} }
    }
    use 'cormacrelf/vim-colors-github'

    -- language plugins
    use 'cespare/vim-toml'
    use 'evanleck/vim-svelte'
    use 'hashivim/vim-terraform'
    use 'lepture/vim-velocity'

    if vim.g.completion_framework == "coc" then
        use { 'neoclide/coc.nvim', branch = 'release' }
        use 'fannheyward/telescope-coc.nvim'
    elseif vim.g.completion_framework == 'coq' then
        use { 'ms-jpq/coq_nvim', branch = 'coq' }
    elseif vim.g.completion_framework == 'ale' then
        use 'dense-analysis/ale'
        use 'fatih/vim-go'
    elseif vim.g.completion_framework == 'nvim' then
        use 'neovim/nvim-lspconfig'
        use 'nvim-lua/lsp_extensions.nvim'
        use 'hrsh7th/nvim-cmp'
        use 'hrsh7th/cmp-nvim-lsp'
        use 'hrsh7th/cmp-buffer'
        use 'hrsh7th/cmp-vsnip'
        use 'hrsh7th/vim-vsnip'
        use 'josa42/nvim-lightline-lsp'
        use { 'psf/black', branch = 'stable' }
        use 'fatih/vim-go'
    end

    if vim.g.include_treesitter == 1 then
        use { 'nvim-treesitter/nvim-treesitter', branch = '0.5-compat', run = ':TSUpdate' }
        use { 'nvim-treesitter/nvim-treesitter-textobjects', branch = '0.5-compat' }
        use 'nvim-treesitter/playground'
    end
end,
config = {}})

if vim.g.completion_framework == "nvim" then
    local cmp = require("cmp")
    cmp.setup({
        snippet = {
            expand = function(args)
                vim.fn['vsnip#anonymous'](args.body)
            end,
        },
        mapping = {
            ['<C-d>'] = cmp.mapping.scroll_docs(-4),
            ['<C-f>'] = cmp.mapping.scroll_docs(4),
            ['<C-Space>'] = cmp.mapping.complete(),
            ['<C-e>'] = cmp.mapping.close(),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
        },
        sources = {
            { name = 'vsnip' },
            { name = 'nvim_lsp' },
            { name = 'buffer' },
        },
    })
end
