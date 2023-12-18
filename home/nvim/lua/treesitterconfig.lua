local ts = require('nvim-treesitter.configs')

ts.setup {
    highlight = {
        enable = true,
        use_languagetree = true,
        disable = {
            "gitcommit",
        },
    },
    indent = {
        enable = false,
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
            include_surrounding_whitespace = function(query_string, selection_mode)
                return query_string == "@function.outer" or query_string == "@class.outer"
            end,
        },
    },
}
