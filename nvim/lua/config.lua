require('disables')
require('settings')
require('plugins')
require('mappings')
require('customlspconfig')
if vim.g.include_treesitter == 1 then
    require('treesitterconfig')
end
require('completion')

-- plugins
require('fzf')
require('vim-test')
require('vtr')
require('fugitive')
if vim.g.include_treesitter == 1 then
    require('treesitter-unit-config')
end
require('telescopeconfig')
