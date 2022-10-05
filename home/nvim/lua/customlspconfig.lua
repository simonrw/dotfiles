local target_servers = {"pyright", "gopls", "rust_analyzer", "yamlls", "terraformls", "efm", "clangd", "rnix", "tsserver"}
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
