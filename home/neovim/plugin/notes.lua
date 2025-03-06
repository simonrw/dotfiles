-- Plugin to add a `TodoItems` command to markdown buffers, which greps for items like
-- - [ ] Do the thing #TODO

local function find_todo_items()
  vim.cmd([[vimgrep '\v[-*]\s+\[\s+\].+(#TODO)' %]])
  vim.cmd.copen()
end

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = vim.api.nvim_create_augroup("Notes", {}),
  callback = function()
    vim.api.nvim_buf_create_user_command(0, "TodoItems", find_todo_items, {})
  end,
})
