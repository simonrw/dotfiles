return {
  {
    "sindrets/diffview.nvim",
    opts = {
      use_icons = false,
      hooks = {
        diff_buf_read = function(bufnr)
          vim.opt_local.number = false
          vim.opt_local.relativenumber = false
        end,
      },
    },
  }
}
