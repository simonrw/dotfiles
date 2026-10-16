-- Fold JSON containers by syntax, not indentation. Keep the root visible.
vim.opt_local.foldmethod = 'expr'
vim.opt_local.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
vim.opt_local.foldlevel = 1
vim.opt_local.foldenable = true
vim.opt_local.foldminlines = 0
vim.opt_local.foldcolumn = 'auto:3'
vim.opt_local.fillchars:append({ fold = ' ' })
vim.opt_local.foldtext = [[getline(v:foldstart) . ' ... ' . trim(getline(v:foldend)) . ' (' . (v:foldend - v:foldstart + 1) . ' lines)']]

vim.keymap.set('n', '<Tab>', 'za', { buffer = true, silent = true, desc = 'Toggle JSON fold' })

vim.b.undo_ftplugin = (vim.b.undo_ftplugin or '')
    .. '|setlocal foldmethod< foldexpr< foldlevel< foldenable< foldminlines< foldcolumn< fillchars< foldtext<'
    .. '|silent! nunmap <buffer> <Tab>'

-- override the format command to pipe through jq
vim.keymap.set('n', '<leader>y', function()
    vim.cmd("%!jq .")
end, { noremap = true, silent = true, buf = 0 })
