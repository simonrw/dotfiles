local mappings = require('mappings')

mappings.nnoremap('<leader>f', [[<cmd>lua require('telescope.builtin').git_files({previewer = false})<Cr>]])
mappings.nnoremap('<leader>F', [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<Cr>]])
mappings.nnoremap('<leader>b', [[<cmd>lua require('telescope.builtin').buffers()<Cr>]])
mappings.nnoremap('<leader>/', [[<cmd>lua require('telescope.builtin').live_grep()<Cr>]])

require("telescope").setup({
  extensions = {
    ["ui-select"] = {},
    ["tele_tabby"] = {},
    fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
    },
  }
})

require("telescope").load_extension("ui-select")
require("telescope").load_extension("tele_tabby")
require("telescope").load_extension("fzf")

mappings.nnoremap('<leader>T', [[<cmd>lua require('telescope').extensions.tele_tabby.list()<Cr>]])
