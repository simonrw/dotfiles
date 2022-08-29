local target_servers = {"pyright", "gopls", "rust_analyzer", "yamlls", "terraformls", "efm", "clangd"}
local lsp_status = require('lsp-status')
local lsp_format = require('lsp-format')
lsp_format.setup {}
lsp_status.register_progress()

function merge_tables(a, b)
    for _, v in pairs(b) do
        table.insert(a, v)
    end
    return a
end

local on_attach = function(client, bufnr, include_formatting)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    if not include_formatting then
        client.resolved_capabilities.document_formatting = false
        client.resolved_capabilities.document_range_formatting = false
    end

    -- mappings
    local opts = { noremap=true, silent=true }

    buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_set_keymap("n", "<leader>d", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)
    buf_set_keymap("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_set_keymap("n", "ge", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "]g", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
    buf_set_keymap("n", "[g", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
    buf_set_keymap("n", "<leader>ca", [[<cmd>lua vim.lsp.buf.code_action({ source = { organizeImports = true }})<CR>]], opts)
    buf_set_keymap("n", "<leader>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
    buf_set_keymap("n", "<leader>D", "<cmd>Telescope diagnostics<cr>", opts)

    buf_set_keymap("n", "<C-Space>", [[<Plug>(completion_trigger)]], opts)

    lsp_format.on_attach(client)

    return lsp_status.on_attach(client)
end

local function setup()
    local lspconfig = require("lspconfig")
    local lsp_installer = require("nvim-lsp-installer")
    local capabilities = vim.tbl_extend('keep',
            require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
            lsp_status.capabilities
            )

    -- Install required servers
    for _, target_server in ipairs(target_servers) do
        _, server = lsp_installer.get_server(target_server)
        if not server:is_installed() then
            print("Installing LSP: " .. target_server)
            server:install(nil)
        end
    end

    lsp_installer.on_server_ready(function(server)
        local opts = {
            on_attach = function(client, bufnr)
                if server.name == "tsserver" then
                    return on_attach(client, bufnr, false)
                else
                    return on_attach(client, bufnr, true)
                end
            end,
            capabilities = capabilities,
            settings = {
                ["rust-analyzer"] = {
                    checkOnSave = {
                        command = "clippy",
                    },
                },
                redhat = { telemetry = { enabled = false } },
                yaml = {
                    schemas = {
                        ["https://raw.githubusercontent.com/awslabs/goformation/v5.2.11/schema/cloudformation.schema.json"] = "/cloudformation*",
                    },
                    customTags = {
                        "!fn",
                        "!And",
                        "!If",
                        "!Not",
                        "!Equals",
                        "!Or",
                        "!FindInMap sequence",
                        "!Base64",
                        "!Cidr",
                        "!Ref",
                        "!Ref Scalar",
                        "!Sub",
                        "!GetAtt",
                        "!GetAZs",
                        "!ImportValue",
                        "!Select",
                        "!Split",
                        "!Join sequence"
                    },
                },
            },
        }

        if server.name == "efm" then
            opts.init_options = {
                documentFormatting = true,
            }
            opts.filetypes = {"lua", "python"}
        end

        -- configure yamlls to include cloudformation tags
        if server.name == "yamlls" then
            opts.filetypes = { "cloudformation", "yaml", "yaml.docker-compose" }
        end

        server:setup(opts)
    end)

    vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
        vim.lsp.diagnostic.on_publish_diagnostics, {
            virtual_text = true,
            signs = true,
            update_in_insert = true,
        }
    )

end

if vim.g.completion_framework == 'nvim' then
    setup()
end

