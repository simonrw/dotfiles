return {
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            {
                "folke/lazydev.nvim",
                ft = "lua", -- only load on lua files
                opts = {
                    library = {
                        -- See the configuration section for more details
                        -- Load luvit types when the `vim.uv` word is found
                        {path = "${3rd}/luv/library", words = {"vim%.uv"}}
                    }
                }
            },
        },
        config = function()
            local lspServers = {
                {
                    extraOptions = {
                        filetypes = {
                            "javascript", "javascriptreact", "javascript.jsx",
                            "typescript", "typescriptreact", "typescript.tsx"
                        },
                        settings = {
                            javascript = {format = {indentSize = 2}},
                            typescript = {format = {indentSize = 2}}
                        }
                    },
                    name = "ts_ls"
                }, {
                    extraOptions = {
                        on_attach = function(client, bufnr)
                            client.config.settings.useLibraryCodeForTypes =
                                false
                            client.config.settings.autoSearchPaths = false
                            client.config.settings
                                .reportTypedDictNotRequiredAccess = "warning"
                            client.config.settings.reportGeneralTypeIssues =
                                "warning"
                            client.config.settings.reportUnusedCallResult =
                                false
                            client.config.settings.reportAny = false
                            client.config.settings.reportOptionalMemberAccess =
                                false
                            client.config.settings.reportUnknownMemberType =
                                false
                            client.config.settings.reportUnknownArgumentType =
                                false
                            client.config.settings.reportUnknownVariableType =
                                fals
                        end
                    },
                    name = "pyright"
                },
                {name = "gopls"},
                {name = "gleam"},
                {name = "elmls"},
                {name = "lua_ls"},
                {name = "zls"},
                {name = "terraformls"},
            }

            local setup = {
                on_attach = function(client, bufnr) end,
            }

            for _, server in ipairs(lspServers) do
                if type(server) == "string" then
                    require("lspconfig")[server].setup(setup)
                else
                    local options = server.extraOptions

                    if options == nil then
                        options = setup
                    else
                        options = vim.tbl_extend("keep", options, setup)
                    end

                    require("lspconfig")[server.name].setup(options)
                end
            end

            -- require("lspconfig").lua_ls.setup {}
            -- require("lspconfig").ts_ls.setup {}
            -- require("lspconfig").rust_analyzer.setup {}
            --
            local __binds = {
                {
                    action = vim.lsp.buf.type_definition,
                    key = "gy",
                    mode = "n",
                    options = {noremap = true, silent = true}
                }, {
                    action = vim.lsp.buf.implementation,
                    key = "gi",
                    mode = "n",
                    options = {noremap = true, silent = true}
                }, {
                    action = vim.lsp.buf.hover,
                    key = "<leader>k",
                    mode = "n",
                    options = {noremap = true, silent = true}
                }, {
                    action = vim.lsp.buf.rename,
                    key = "<leader>r",
                    mode = "n",
                    options = {noremap = true, silent = true}
                }, {
                    action = vim.diagnostic.goto_next,
                    key = "]d",
                    mode = "n",
                    options = {noremap = true, silent = true}
                }, {
                    action = vim.diagnostic.goto_prev,
                    key = "[d",
                    mode = "n",
                    options = {noremap = true, silent = true}
                }, {
                    action = function()
                        vim.lsp.buf.code_action({
                            source = {organizeImports = true}
                        })
                    end,
                    key = "<leader>a",
                    mode = "n",
                    options = {noremap = true, silent = true}
                }, {
                    action = vim.lsp.buf.signature_help,
                    key = "<C-h>",
                    mode = "i",
                    options = {noremap = true, silent = true}
                }
            }

            for _, map in ipairs(__binds) do
                vim.keymap.set(map.mode, map.key, map.action, map.options)
            end

            vim.lsp.inlay_hint.enable()
        end
    }
}
