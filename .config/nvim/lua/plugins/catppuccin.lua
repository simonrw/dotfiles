return {

  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    lazy = false,
    enabled = vim.g.is_dark_mode,
    config = function()
      require("catppuccin").setup()

      vim.cmd.colorscheme "catppuccin-mocha"
      vim.cmd.highlight({ "TreesitterContextBottom", "gui=none" })
    end
  },
}
