-- imports
local lsp = require('lsp-zero')
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
    timer:start(200, 0, vim.schedule_wrap(function()
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
