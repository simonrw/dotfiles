return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-neotest/neotest-python",
    },
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require("neotest").setup({
        adapters = {
          require("neotest-python")({
            dap = { justMyCode = false },
          }),
          require('rustaceanvim.neotest'),
          -- TODO
          -- require("neotest-vim-test")({
          --   ignore_file_types = { "python", "vim", "lua" },
          -- }),
        },
        output_panel = {
          enabled = true,
          open = "tabnew",
        },
        ---@diagnostic disable-next-line: missing-fields
        output = {
          enabled = false,
        },
      })

      -- setup keybindings
      vim.keymap.set("n", "tf", function() require("neotest").run.run(vim.fn.expand("%")) end,
        { noremap = true, silent = true, desc = "Test the current file" })
      vim.keymap.set("n", "tl", function() require("neotest").run.run_last() end,
        { noremap = true, silent = true, desc = "Test the last executed test" })
      vim.keymap.set("n", "tn", function() require("neotest").run.run() end,
        { noremap = true, silent = true, desc = "Run the nearest test" })
      vim.keymap.set("n", "to", function() require("neotest").output_panel.toggle() end,
        { noremap = true, silent = true, desc = "Toggle test output panel" })
    end,
  }
}
