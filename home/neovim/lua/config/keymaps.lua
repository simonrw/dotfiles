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
      options = { noremap = true, silent = true, desc = "Format the current buffer" },
    },
    {
      action = ":0,$y+<cr>",
      key = "cp",
      mode = "n",
      options = { noremap = true, silent = true, desc = "Copy the file into the system clipboard" }
    }, {
    action = "<C-\\><C-n>",
    key = "<Esc>",
    mode = "t",
    options = { noremap = true, silent = true, desc = "Enter normal mode when in a terminal buffer" }
  }, {
    action = "<Esc>",
    key = "<M-[>",
    mode = "t",
    options = { noremap = true, silent = true, desc = "Enter normal mode when in a terminal buffer" }
  }, {
    action = "<Esc>",
    key = "<C-v><Esc>",
    mode = "t",
    options = { noremap = true, silent = true, desc = "Enter normal mode when in a terminal buffer" }
  }, {
    action = ':mksession!|echo "Session saved"<cr>',
    key = "<leader>W",
    mode = "n",
    options = { noremap = true, silent = true, desc = "Save the current session" }
  }, {
    action = ":ToggleList<cr>",
    key = "Q",
    mode = "n",
    options = { noremap = true, silent = true, desc = "Toggle the quickfix list" }
  }, {
    action = "<C-w><C-h>",
    key = "<C-h>",
    mode = "n",
    options = { noremap = true, silent = true}
  }, {
    action = "<C-w><C-j>",
    key = "<C-j>",
    mode = "n",
    options = { noremap = true, silent = true}
  }, {
    action = "<C-w><C-k>",
    key = "<C-k>",
    mode = "n",
    options = { noremap = true, silent = true}
  }, {
    action = "<C-w><C-l>",
    key = "<C-l>",
    mode = "n",
    options = { noremap = true, silent = true}
  }, {
    action = "nzzzv",
    key = "n",
    mode = "n",
    options = { noremap = true, silent = true}
  }, {
    action = "Nzzzv",
    key = "N",
    mode = "n",
    options = { noremap = true, silent = true}
  }, {
    action = "/\\v",
    key = "/",
    mode = "n",
    options = { noremap = true, silent = true}
  }, {
    action = "?\\v",
    key = "?",
    mode = "n",
    options = { noremap = true, silent = true }
  }, {
    action = "<esc>",
    key = "jk",
    mode = "i",
    options = { noremap = true, silent = true, desc = "Map jk to escape" }
  }, {
    action = "<nop>",
    key = "<esc>",
    mode = "i",
    options = { noremap = true, silent = true, desc = "Disable escape" }
  }
  }
  for _, map in ipairs(__binds) do
    vim.keymap.set(map.mode, map.key, map.action, map.options)
  end
end
