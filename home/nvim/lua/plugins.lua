-- bootstrap packer
local execute = vim.api.nvim_command
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ 'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path })
    execute('packadd packer.nvim')
end

require('packer').startup({
    function(use)
        use 'wbthomason/packer.nvim'
        use 'christoomey/vim-tmux-runner'
        use 'christoomey/vim-conflicted'

        -- language plugins
        use 'lepture/vim-velocity'

        -- load telescope in here so we get the latest and greatest
        use {
            'nvim-telescope/telescope.nvim',
            setup = function()
                local mappings = require('mappings')

                mappings.nnoremap('<leader>f', [[<cmd>lua require('telescope.builtin').git_files()<Cr>]])
                mappings.nnoremap('<leader>F', [[<cmd>lua require('telescope.builtin').find_files()<Cr>]])
                mappings.nnoremap('gb', [[<cmd>lua require('telescope.builtin').buffers()<Cr>]])
                mappings.nnoremap('<leader><space>', [[<cmd>lua require('telescope.builtin').live_grep()<Cr>]])
                mappings.nnoremap('<leader>/', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<Cr>]])

                require("telescope").setup({
                    defaults = {
                        layout_strategy = 'horizontal',
                        layout_config = {
                            prompt_position = 'top',
                        },
                        sorting_strategy = 'ascending',
                    },
                    pickers = {
                        git_files = {
                            disable_devicons = true,
                        },
                        find_files = {
                            disable_devicons = true,
                        },
                    },
                    extensions = {
                        fzf = {
                            fuzzy = true,
                            override_generic_sorter = true,
                            override_file_sorter = true,
                            case_mode = "smart_case",
                        },
                    }
                })

                require("telescope").load_extension("fzf")
            end,
        }

        use 'averms/black-nvim'
        use 'lewis6991/gitsigns.nvim'

        use {
            'nvim-treesitter/nvim-treesitter-context',
            config = function()
                require('treesitter-context').setup({
                    max_lines = 5,
                })
            end,
        }

        use {
            'VonHeikemen/lsp-zero.nvim',
            branch = "v2.x",
            event = {
                "BufRead",
                "BufNew",
            },
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
            },
            config = function()
                -- imports
                local lsp = require('lsp-zero')
                local lsp_status = require('lsp-status')

                lsp_status.register_progress()

                lsp.set_preferences({
                    suggest_lsp_servers = true,
                    setup_servers_on_start = true,
                    set_lsp_keymaps = false,
                    configure_diagnostics = true,
                    cmp_capabilities = true,
                    manage_nvim_cmp = true,
                    call_servers = 'local',
                    sign_icons = {
                        error = '✘',
                        warn = '▲',
                        hint = '⚑',
                        info = ''
                    }
                })

                local cmp = require('cmp')
                local cmp_select = { behaviour = cmp.SelectBehavior.Select }
                local cmp_mappings = lsp.defaults.cmp_mappings({
                    ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
                    ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
                    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
                    ['<C-space>'] = cmp.mapping.complete(),
                    ['<C-e>'] = cmp.config.disable,
                })

                lsp.on_attach(function(client, bufnr)
                    -- mappings
                    local opts = { buffer = bufnr, remap = false }

                    -- disable formatting for docker language servers
                    if client.name == 'dockerls' or client.name == "docker_compose_language_service" then
                        client.server_capabilities.documentFormattingProvider = false
                    end

                    -- match helix bindings
                    vim.keymap.set("n", "gd", function()
                        require('telescope.builtin').lsp_definitions()
                    end, opts)
                    vim.keymap.set("n", "gy", function() vim.lsp.buf.type_definition() end, opts)
                    vim.keymap.set("n", "gr", function() require('telescope.builtin').lsp_references() end, opts)
                    vim.keymap.set("n", "gi", function() vim.lsp.buf.implementation() end, opts)
                    vim.keymap.set("n", "<leader>k", function() vim.lsp.buf.hover() end, opts)
                    vim.keymap.set("n", "<leader>r", function() vim.lsp.buf.rename() end, opts)
                    vim.keymap.set("n", "]d", function() vim.diagnostic.goto_next() end, opts)
                    vim.keymap.set("n", "[d", function() vim.diagnostic.goto_prev() end, opts)
                    vim.keymap.set("n", "<leader>a", function() vim.lsp.buf.code_action({ source = { organizeImports = true } }) end,
                        opts)
                    vim.keymap.set("n", "<leader>g", function() require('telescope.builtin').diagnostics() end, opts)
                    vim.keymap.set("n", "<leader>s", function() require('telescope.builtin').lsp_dynamic_workspace_symbols() end, opts)
                    vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)

                    vim.keymap.set("n", "<C-Space>", [[<Plug>(completion_trigger)]], opts)

                    return lsp_status.on_attach(client)
                end)


                lsp.set_preferences({
                    sign_icons = {}
                })

                -- remove snippets
                lsp.setup_nvim_cmp({
                    mapping = cmp_mappings,
                    preselect = 'none',
                    completion = {
                        completeopt = 'menu,menuone,noinsert,noselect',
                        autocomplete = false,
                    },
                    sources = {
                        { name = 'path' },
                        { name = 'nvim_lsp' },
                        { name = 'buffer',  keyword_length = 3 },
                    }
                })

                lsp.setup()

                -- timer code
                local timer = vim.loop.new_timer()
                local completion_timer = function()
                    timer:stop()
                    timer:start(100, 0, vim.schedule_wrap(function()
                        cmp.complete({ reason = cmp.ContextReason.Auto })
                    end))
                end


                -- configure delay before completion
                local augroup = vim.api.nvim_create_augroup("completion", { clear = true })
                vim.api.nvim_create_autocmd({ "TextChangedI" }, {
                    pattern = {"*"},
                    group = augroup,
                    callback = completion_timer,
                })

                -- must come after lsp.setup
                vim.diagnostic.config({
                    virtual_text = true,
                })
            end,
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
            end,
            event = {
                "BufRead",
                "BufNew",
            },
        }
    end,
    config = {}
})
