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
    use 'christoomey/vim-tmux-runner'
    use 'christoomey/vim-conflicted'

    -- language plugins
    use 'lepture/vim-velocity'
    use 'tweekmonster/django-plus.vim'

    use "williamboman/mason.nvim" 
    use 'williamboman/mason-lspconfig.nvim'

    use 'averms/black-nvim'

    use {
        'pwntester/octo.nvim',
        requires = {
            'nvim-lua/plenary.nvim',
            'nvim-telescope/telescope.nvim',
        },
        config = function()
            require('octo').setup({
                file_panel = {
                    use_icons = false,
                },
            })
        end,
    }

    -- debugging
    use 'leoluz/nvim-dap-go'
    use 'mfussenegger/nvim-dap-python'
end,
config = {}})
