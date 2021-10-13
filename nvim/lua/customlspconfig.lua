local function setup()
    vim.o.completeopt = "menuone,noinsert,noselect"

    local lspconfig = require("lspconfig")


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
        buf_set_keymap("n", "]g", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
        buf_set_keymap("n", "[g", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
        buf_set_keymap("n", "<leader>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)

        buf_set_keymap("n", "<C-Space>", [[<Plug>(completion_trigger)]], opts)

        vim.cmd("command! LspFormatting lua vim.lsp.buf.formatting()")
    end

    local rust_analyzer_on_attach = function(client, bufnr)
        on_attach(client, bufnr)

        vim.cmd([[
            augroup RustAutoSave
                au!
                au BufWritePre *.rs lua vim.lsp.buf.formatting()
            augroup END
        ]])
    end

    -- lspconfig
    local servers = {"pyright", "gopls", "ccls"}
    for _, lsp in ipairs(servers) do
        lspconfig[lsp].setup {
            on_attach = on_attach,
            flags = {
                debounce_text_changes = 150,
            },
            capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
        }
    end

    lspconfig["rust_analyzer"].setup {
        on_attach = rust_analyzer_on_attach,
        capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
        flags = {
            debounce_text_changes = 150,
        },
        settings = {
            ["rust-analyzer"] = {
                assist = {
                    importGranularity = "module",
                    importPrefix = "by_self",
                },
                cargo = {
                    loadOutDirsFromCheck = true
                },
                procMacro = {
                    enable = true
                },
            },
        },
    }

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

