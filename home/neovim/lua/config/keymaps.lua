do
    local __binds = {
        -- { action = ":update|:TestFile<cr>", key = "tf", mode = "n", options = { noremap = true, silent = true } },
        -- { action = ":update|:TestLast<cr>", key = "tl", mode = "n", options = { noremap = true, silent = true } },
        -- { action = ":update|:TestNearest<cr>", key = "tn", mode = "n", options = { noremap = true, silent = true } },
        -- { action = ":update|:TestSuite<cr>", key = "ta", mode = "n", options = { noremap = true, silent = true } },
        -- { action = ":update|:TestSuite<cr>", key = "ts", mode = "n", options = { noremap = true, silent = true } },
        {
          action = function() vim.lsp.buf.format() end,
          key = "<leader>y",
          mode = "n",
          options = {noremap = true, silent = true},
        },
        {
            action = ":0,$y+<cr>",
            key = "cp",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "<C-\\><C-n>",
            key = "<Esc>",
            mode = "t",
            options = {noremap = true, silent = true}
        }, {
            action = "<Esc>",
            key = "<M-[>",
            mode = "t",
            options = {noremap = true, silent = true}
        }, {
            action = "<Esc>",
            key = "<C-v><Esc>",
            mode = "t",
            options = {noremap = true, silent = true}
        }, {
            action = ':mksession!|echo "Session saved"<cr>',
            key = "<leader>W",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = ":ToggleList<cr>",
            key = "Q",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = ":e %:h<Cr>",
            key = "-",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "<C-w><C-h>",
            key = "<C-h>",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "<C-w><C-j>",
            key = "<C-j>",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "<C-w><C-k>",
            key = "<C-k>",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "<C-w><C-l>",
            key = "<C-l>",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "nzzzv",
            key = "n",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "Nzzzv",
            key = "N",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "/\\v",
            key = "/",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "?\\v",
            key = "?",
            mode = "n",
            options = {noremap = true, silent = true}
        }, {
            action = "<cmd>Lspsaga outline<cr>",
            key = "<leader>A",
            mode = "n",
            options = {noremap = true, silent = true}
        }
    }
    for _, map in ipairs(__binds) do
        vim.keymap.set(map.mode, map.key, map.action, map.options)
    end
end
