if has('nvim-0.5')
    Plug 'neovim/nvim-lspconfig'
    Plug 'hrsh7th/nvim-compe'
    Plug 'RishabhRD/popfix'
    Plug 'RishabhRD/nvim-lsputils'
    Plug 'nvim-lua/lsp-status.nvim'
endif

" lsp configuration
if has('nvim-0.5')
    luafile ~/.vim/lua/customlspconfig.lua
endif

