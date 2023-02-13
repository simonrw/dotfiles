-- imports
local lsp = require('lsp-zero')
local lsp_format = require('lsp-format')
local lsp_signature = require('lsp_signature')

lsp_format.setup {}

lsp.preset('recommended')

local cmp = require('cmp')
local cmp_select = { behaviour = cmp.SelectBehavior.Select }
local cmp_mappings = lsp.defaults.cmp_mappings({
    ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
    ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
    ['<C-space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.config.disable,
})

lsp.setup_nvim_cmp({
    mapping = cmp_mappings,
})

local signature_setup = {
    handler_opts = {
        border = "none",
    },
}

lsp.on_attach(function(client, bufnr)
    -- mappings
    local opts = { buffer = bufnr, remap = false }

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

    lsp_signature.on_attach(signature_setup, bufnr)

    return lsp_format.on_attach(client)
end)


lsp.set_preferences({
    sign_icons = {}
})

-- remove snippets
lsp.setup_nvim_cmp({
    sources = {
        { name = 'path' },
        { name = 'nvim_lsp' },
        { name = 'buffer', keyword_length = 3 },
    }
})

-- configure lua lsp
lsp.configure('sumneko_lua', {
    settings = {
        Lua = {
            diagnostics = {
                globals = { "vim" },
            },
        },
    },
})

lsp.setup()

-- must come after lsp.setup
vim.diagnostic.config({
    virtual_text = true,
})

require('fidget').setup({
    align = {
        bottom = false,
    },
    window = {
        blend = 0,
    },
})
