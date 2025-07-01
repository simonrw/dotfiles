return {

  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    lazy = false,
    enabled = vim.g.is_dark_mode,
    config = function()
      require("catppuccin").setup()

      vim.cmd.colorscheme "catppuccin-macchiato"
      vim.cmd.highlight({ "TreesitterContextBottom", "gui=none" })
      vim.cmd.highlight({ "CursorLine", "guibg=#303347" })
      vim.cmd.highlight({ "CursorColumn", "guibg=#303347" })
    end
  },
}
