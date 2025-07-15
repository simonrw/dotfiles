vim.o.undofile      = true
vim.o.laststatus    = 0
vim.opt.expandtab   = true
vim.opt.shiftwidth  = 4
vim.opt.softtabstop = -1
vim.cmd("syntax off | colorscheme retrobox | highlight Normal guifg=#ffaf00 guibg=#282828")
vim.keymap.set('n', '<space>y', function() vim.fn.setreg('+', vim.fn.expand('%:p')) end)
vim.keymap.set("n", "<space>c", function()
    vim.ui.input({}, function(c)
        if c and c ~= "" then
            vim.cmd("noswapfile vnew")
            vim.bo.buftype = "nofile"
            vim.bo.bufhidden = "wipe"
            vim.api.nvim_buf_set_lines(0, 0, -1, false, vim.fn.systemlist(c))
        end
    end)
end)
