return {
  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require('gitsigns').setup()

      -- keymaps
      local __binds = {
        {
          action = function()
            if vim.wo.diff then return "]c" end
            vim.schedule(function()
              require("gitsigns").next_hunk()
            end)
            return "<Ignore>"
          end,
          key = "]c",
          mode = "n",
          options = { expr = true, silent = true, desc = "Go to next change" }
        }, {
        action = function()
          if vim.wo.diff then return "]c" end
          vim.schedule(function()
            require("gitsigns").prev_hunk()
          end)
          return "<Ignore>"
        end,
        key = "[c",
        mode = "n",
        options = { expr = true, silent = true, desc = "Go to previous change" }
      }, {
        action = require("gitsigns").preview_hunk,
        key = "<leader>hp",
        mode = "n",
        options = { silent = true, desc = "Preview hunk under the cursor" }
      }, {
        action = require("gitsigns").reset_hunk,
        key = "<leader>hr",
        mode = "n",
        options = { silent = true, desc = "Reset hunk under the cursor" }
      }, {
        action = require("gitsigns").stage_hunk,
        key = "<leader>hs",
        mode = "n",
        options = { silent = true, desc = "Stage hunk under the cursor" }
      }, {
        action = require("gitsigns").undo_stage_hunk,
        key = "<leader>hu",
        mode = "n",
        options = { silent = true, desc = "Unstage hunk under the cursor" }
      }
      }
      for _, map in ipairs(__binds) do
        vim.keymap.set(map.mode, map.key, map.action, map.options)
      end
    end
  }
}
