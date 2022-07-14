require('disables')
require('settings')
require('plugins')
require('mappings')
require('gutentags')
require('customlspconfig')
if vim.g.include_treesitter == 1 then
    require('treesitterconfig')
end
require('completionconfig')

-- plugins
require('fzf')
require('vim-test')
require('vtr')
require('fugitive')
if vim.g.include_treesitter == 1 then
    require('treesitter-unit-config')
end
require('telescopeconfig')
require('dapconfig')
require('lualineconfig')
