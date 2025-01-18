return {
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    ---@type snacks.Config
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
      bigfile = { enabled = true },
      input = { enabled = true },
      notifier = { enabled = true },
      quickfile = { enabled = true },
      words = { enabled = true },
    },
    keys = {
      { "<leader>f", function() require("snacks.picker").git_files() end, desc = "Find git files" },
      { "<leader>F", function() require("snacks.picker").files() end, desc = "Find files" },
      { "<leader>d", function() require("snacks.picker").diagnostics() end, desc = "LSP diagnostics" },
      { "<leader>S", function() require("snacks.picker").lsp_symbols() end, desc = "LSP symbols" },
      { "gb", function() require("snacks.picker").buffers() end, desc = "Open buffers" },
      { "gd", function() require("snacks.picker").lsp_definitions() end, desc = "LSP definitions" },
      { "gr", function() require("snacks.picker").lsp_references() end, desc = "LSP references" },
      { "gi", function() require("snacks.picker").lsp_implementations() end, desc = "LSP implementations" },
      { "gy", function() require("snacks.picker").lsp_type_definitions() end, desc = "LSP type definition" },
      { "<leader>j", function() require("snacks.picker").jumps() end, desc = "Jump list" },
      { "<leader>ht", function() require("snacks.picker").help() end, desc = "Help tags" },
      { "<leader><space>", function() require("snacks.picker").grep() end, desc = "Perform a live grep over the project" },
    },
    config = function()
      vim.api.nvim_create_autocmd("User", {
        pattern = "MiniFilesActionRename",
        callback = function(event)
          Snacks.rename.on_rename_file(event.data.from, event.data.to)
        end,
      })
    end,
  }
}
