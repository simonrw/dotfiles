local bufnr = vim.api.nvim_get_current_buf()

vim.keymap.set(
  "n",
  "<leader>a",
  function()
    vim.cmd.RustLsp('codeAction') -- supports rust-analyzer's grouping
    -- or vim.lsp.buf.codeAction() if you don't want grouping.
  end,
  { silent = true, buffer = bufnr, desc = "Rust code action" }
)

vim.keymap.set(
  "n",
  "K", -- Override Neovim's built-in hover keymap with rustaceanvim's hover actions
  function()
    vim.cmd.RustLsp({ 'hover', 'actions' })
  end,
  { silent = true, buffer = bufnr, desc = "Rust hover" }
)

vim.keymap.set(
  "n",
  "<leader>K",
  function()
    vim.cmd.RustLsp('openDocs')
  end,
  { silent = true, buffer = bufnr, desc = "Rust hover" }
)

