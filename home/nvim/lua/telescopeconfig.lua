local mappings = require('mappings')

mappings.nnoremap('<leader>f', [[<cmd>lua require('telescope.builtin').git_files()<Cr>]])
mappings.nnoremap('<leader>F', [[<cmd>lua require('telescope.builtin').find_files()<Cr>]])
mappings.nnoremap('gb', [[<cmd>lua require('telescope.builtin').buffers()<Cr>]])
mappings.nnoremap('<leader>/', [[<cmd>lua require('telescope.builtin').live_grep()<Cr>]])

require("telescope").setup({
    defaults = {
        layout_strategy = 'horizontal',
        layout_config = {
            prompt_position = 'top',
        },
        sorting_strategy = 'ascending',
    },
    extensions = {
        ["ui-select"] = {},
        fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
        },
    }
})

require("telescope").load_extension("ui-select")
require("telescope").load_extension("fzf")
