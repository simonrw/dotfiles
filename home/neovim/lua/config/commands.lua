vim.api.nvim_create_user_command("T", "split | resize 30 | term <args>", {
    complete = "shellcmd",
    force = true,
    nargs = "*",
})

vim.api.nvim_create_user_command("ToggleList", function()
  local qf_exists = false
  for _, win in pairs(vim.fn.getwininfo()) do
    if win["quickfix"] == 1 then
      qf_exists = true
    end
  end

  if qf_exists then
    vim.cmd "cclose"
  else
    vim.cmd "copen"
  end
end, {})
