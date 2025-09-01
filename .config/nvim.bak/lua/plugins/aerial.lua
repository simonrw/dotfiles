return {
  {
    'stevearc/aerial.nvim',
    opts = {},
    -- Optional dependencies
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons"
    },
    config = function()
      local aerial = require("aerial")

      aerial.setup({})

      vim.keymap.set("n", "<leader>A", function()
        aerial.toggle()
      end, { silent = true, noremap = true, desc = "Toggle structure view" })
    end,
  }
}
