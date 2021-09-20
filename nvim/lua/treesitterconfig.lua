local function setup()
    local ts = require('nvim-treesitter.configs')

    ts.setup {
        ensure_installed = 'maintained',
        highlight = {
            enable = true,
            use_languagetree = true,
        },
        indent = {
            enable = true,
        },
        textobjects = {
            select = {
                enable = true,
                lookahead = true,
                keymaps = {
                    ["af"] = "@function.outer",
                    ["if"] = "@function.inner",
                    ["ac"] = "@class.outer",
                    ["ic"] = "@class.inner",
                },
            },
        },
    }
end

if vim.fn.has('nvim-0.5') and vim.g.include_treesitter then
    setup()
end

