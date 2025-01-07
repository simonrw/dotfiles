return {
  {
    "https://github.com/tpope/vim-fugitive",
    config = function()
      local binds = {
        {
          action = ":Git commit -v<cr>",
          key = "<leader>gc",
          mode = "n",
          options = { noremap = true, silent = true, desc = "Git commit" }
        }, {
        action = ":Gvdiff<cr>",
        key = "<leader>gd",
        mode = "n",
        options = { noremap = true, silent = true, desc = "Diff the current file"
        },
        {
          action = ":Gwrite<cr>",
          key = "<leader>gw",
          mode = "n",
          options = { noremap = true, silent = true, desc = "Stage changes in the current file" }
        },
        {
          action = ":Gread<cr>",
          key = "<leader>gr",
          mode = "n",
          options = { noremap = true, silent = true, desc = "Undo changes in the current file" }
        },
        {
          action = ":Git<cr>",
          key = "gs",
          mode = "n",
          options = { noremap = true, silent = true, desc = "Git status" }
        },
        {
          action = ":Git commit -v --amend<cr>",
          key = "<leader>ga",
          mode = "n",
          options = { noremap = true, silent = true, desc = "Ammend the previous commit" }
        }
      }
      }

      for _, map in ipairs(binds) do
        vim.keymap.set(map.mode, map.key, map.action, map.options)
      end

      local autocommands = {
        {
          command = "nmap <buffer> q gq",
          event = { "FileType" },
          pattern = { "fugitive" }
        }
      }

      for _, autocmd in ipairs(autocommands) do
        vim.api.nvim_create_autocmd(autocmd.event, {
          group = autocmd.group,
          pattern = autocmd.pattern,
          buffer = autocmd.buffer,
          desc = autocmd.desc,
          callback = autocmd.callback,
          command = autocmd.command,
          once = autocmd.once,
          nested = autocmd.nested
        })
      end
    end
  },
  "tpope/vim-rhubarb",
}
