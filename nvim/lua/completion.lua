function setup()
    vim.opt.completeopt = { "menu", "menuone", "noselect" }
    local cmp = require('cmp');

    cmp.setup {
        mapping = {
            ["<C-d>"] = cmp.mapping.scroll_docs(-4),
            ["<C-f>"] = cmp.mapping.scroll_docs(4),
            ["<C-e>"] = cmp.mapping.close(),
            ["<c-y>"] = cmp.mapping.confirm {
                behavior = cmp.ConfirmBehavior.Insert,
                select = true,
            },
            ["<c-q>"] = cmp.mapping.confirm {
                behavior = cmp.ConfirmBehavior.Replace,
                select = true,
            },
            ["<c-space>"] = cmp.mapping.complete(),
        },
        sources = {
            { name = "nvim_lua" },
            { name = "nvim_lsp" },
            { name = "path" },
            { name = "luasnip" },
            { name = "buffer", keyword_length = 5, max_item_count = 10 },
        },
        snippet = {
            expand = function(args)
                require("luasnip").lsp_expand(args.body)
            end,
        },
        experimental = {
            native_menu = false,
            ghost_text = true,
        },
    }

end

if vim.g.completion_framework == 'nvim' then
    setup()
end


