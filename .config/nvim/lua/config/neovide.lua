local map = vim.keymap

-- neovide specific settings
if vim.g.neovide then
  vim.g.neovide_cursor_animation_length = 0
  vim.g.neovide_cursor_vfx_mode = ""
  vim.g.neovide_scroll_animation_length = 0

  -- bindings to change font size
  map.set({ "n", "v" }, "<C-+>", function() vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + 0.1 end)
  map.set({ "n", "v" }, "<C-->", function() vim.g.neovide_scale_factor = vim.g.neovide_scale_factor - 0.1 end)
  map.set({ "n", "v" }, "<C-0>", function() vim.g.neovide_scale_factor = 1 end)

  -- bindings for copy/paste
  map.set(
    { 'n', 'v', 's', 'x', 'o', 'i', 'l', 'c', 't' },
    '<D-v>',
    function() vim.api.nvim_paste(vim.fn.getreg("+"), true, -1) end,
    { noremap = true, silent = true }
  )

  vim.o.guifont = "Lilex Medium"
end
