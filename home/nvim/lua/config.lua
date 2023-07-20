require('disables')
require('settings')
require('plugins')
require('mappings')
require('lualineconfig')

if vim.g.include_treesitter == 1 then
    require('treesitterconfig')
end

require('gutentags')
require('hardtime')

-- plugins
require('fzf')
require('vim-test')
require('vtr')
require('fugitive')

require('completionconfig')
require('editorconfigconfig')
require('aerialconfig')

-- debugging
require('dapconfig')
