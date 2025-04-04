return {
  {
    "folke/trouble.nvim",
    opts = {
      auto_preview = false,
      focus = true,
    },
    keys = {
      {
        "<leader>d",
        "<cmd>Trouble diagnostics toggle<cr>",
        desc = "Diagnostics (Trouble)",
      }
    },
  },
}
