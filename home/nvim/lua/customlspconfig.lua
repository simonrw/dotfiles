local target_servers = {"pyright", "gopls", "rust_analyzer", "yamlls", "terraformls", "clangd", "rnix", "tsserver"}
local lsp_status = require('lsp-status')
local lsp_format = require('lsp-format')
lsp_format.setup {}
lsp_status.register_progress()

local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- mappings
    local opts = { noremap=true, silent=true }

    -- match helix bindings
    buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
    buf_set_keymap("n", "<leader>k", "<cmd>lua vim.lsp.buf.hover()<cr>", opts)
    buf_set_keymap("n", "<leader>r", "<cmd>lua vim.lsp.buf.rename()<cr>", opts)
    buf_set_keymap("n", "<leader>r", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
    buf_set_keymap("n", "<leader>i", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
    buf_set_keymap("n", "<leader>y", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
    buf_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
    buf_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
    buf_set_keymap("n", "<leader>a", [[<cmd>lua vim.lsp.buf.code_action({ source = { organizeImports = true }})<CR>]], opts)
    buf_set_keymap("n", "<leader>d", "<cmd>Telescope diagnostics<cr>", opts)
    buf_set_keymap("n", "<leader>s", "<cmd>Telescope lsp_document_symbols<cr>", opts)
    buf_set_keymap("n", "<leader>S", "<cmd>Telescope lsp_workspace_symbols<cr>", opts)

    buf_set_keymap("n", "<C-Space>", [[<Plug>(completion_trigger)]], opts)

    lsp_format.on_attach(client)

    return lsp_status.on_attach(client)
end

require('mason').setup()
require("mason-lspconfig").setup({
    ensure_installed = target_servers,
    automatic_installation = true,
})
require('mason-lspconfig').setup_handlers({
    function (server_name)
        require('lspconfig')[server_name].setup({
            on_attach = on_attach,
        })
    end,
})
