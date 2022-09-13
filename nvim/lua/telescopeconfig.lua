local mappings = require('mappings')

mappings.nnoremap('<leader>f', [[<cmd>lua require('telescope.builtin').git_files({previewer = false})<Cr>]])
mappings.nnoremap('<leader>F', [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<Cr>]])
mappings.nnoremap('gb', [[<cmd>lua require('telescope.builtin').buffers()<Cr>]])
mappings.nnoremap('gl', [[<cmd>lua require('telescope.builtin').live_grep()<Cr>]])
mappings.nnoremap('<leader>gT', [[<cmd>lua require('telescope.builtin').tags()<Cr>]])

if vim.g.completion_framework == 'coc' then
    require('telescope').load_extension('coc')
    mappings.nnoremap('<leader>gt', [[<cmd>Telescope coc workspace_symbols<Cr>]])
elseif vim.g.completion_framework == 'nvim' then
    mappings.nnoremap('<leader>gt', [[<cmd>lua require('telescope.builtin').lsp_workspace_symbols()<Cr>]])
end

require("telescope").setup({
  extensions = {
    ["ui-select"] = {},
    ["tele_tabby"] = {},
  }
})

require("telescope").load_extension("ui-select")
require("telescope").load_extension("tele_tabby")

mappings.nnoremap('<leader>T', [[<cmd>lua require('telescope').extensions.tele_tabby.list()<Cr>]])
