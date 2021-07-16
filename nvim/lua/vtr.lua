nnoremap('<leader>v', ':VtrAttachToPane<cr>')

-- Create shorter 'V' command which is easier to type
vim.cmd([[
command! -nargs=? V call VtrSendCommand(<f-args>)
]])
