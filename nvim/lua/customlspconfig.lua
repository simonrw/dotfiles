local target_servers = {"pyright", "gopls", "rust_analyzer", "yamlls", "terraformls", "hls"}

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- mappings
    local opts = { noremap=true, silent=true }

    buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_set_keymap("n", "<leader>d", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)
    buf_set_keymap("n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
    buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_set_keymap("n", "td", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "]g", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
    buf_set_keymap("n", "[g", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
    buf_set_keymap("n", "<leader>oi", [[<cmd>lua vim.lsp.buf.code_action({ source = { organizeImports = true }})<CR>]], opts)
    buf_set_keymap("n", "<leader>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)

    buf_set_keymap("n", "<C-Space>", [[<Plug>(completion_trigger)]], opts)

    -- autoformat on save
    vim.api.nvim_command("au BufWritePost <buffer> lua vim.lsp.buf.formatting()")

    vim.cmd("command! LspFormatting lua vim.lsp.buf.formatting()")
end

local function setup()
    local lspconfig = require("lspconfig")
    local lsp_installer = require("nvim-lsp-installer")
    local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

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
            on_attach = on_attach,
            flags = {
                debounce_text_changes = 150,
            },
            capabilities = capabilities,
        }

        vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
            vim.lsp.diagnostic.on_publish_diagnostics, {
                virtual_text = true,
                signs = true,
                update_in_insert = true,
            }
        )

        server:setup(opts)
    end)
end

if vim.g.completion_framework == 'nvim' then
    setup()
end

