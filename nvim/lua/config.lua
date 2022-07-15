require('disables')
require('settings')
require('plugins')
require('mappings')
require('gutentags')
if vim.g.include_treesitter == 'nvim' then
    require('customlspconfig')
    require('completionconfig')
    require('dapconfig')
end
if vim.g.include_treesitter == 1 then
    require('treesitterconfig')
end

-- plugins
require('fzf')
require('vim-test')
require('vtr')
require('fugitive')
if vim.g.include_treesitter == 1 then
    require('treesitter-unit-config')
end
require('telescopeconfig')
require('lualineconfig')
