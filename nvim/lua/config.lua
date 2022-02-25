-- set up global function
_G.P = function(any)
    print(vim.inspect(any))
end

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
require('nulllsconfig')

-- plugins
require('fzf')
require('vim-test')
require('vtr')
require('fugitive')
if vim.g.include_treesitter == 1 then
    require('treesitter-unit-config')
end
require('telescopeconfig')
