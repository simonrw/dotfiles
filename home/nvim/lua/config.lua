require('disables')
require('settings')
require('plugins')
require('mappings')
require('telescopeconfig')
require('lualineconfig')

if vim.g.include_treesitter == 1 then
    require('treesitterconfig')
end

require('gutentags')

if vim.g.completion_framework == 'nvim' then
    require('customlspconfig')
    require('completionconfig')
end

-- plugins
require('fzf')
require('vim-test')
require('vtr')
require('fugitive')
require('octoconfig')
