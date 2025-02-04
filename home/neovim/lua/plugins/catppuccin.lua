return {

  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    lazy = false,
    config = function()
      require("catppuccin").setup()

      vim.cmd.background = "dark"
      vim.cmd.colorscheme "catppuccin-macchiato"
      vim.cmd.highlight({ "TreesitterContextBottom", "gui=none" })
    end
  },
}
