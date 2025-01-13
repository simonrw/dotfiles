local buffer_number = nil
local command = nil

local function choose_pane()
  vim.system({ "tmux", "display-panes" }, { text = true }):wait()
  vim.ui.input({ prompt = "Which pane? " }, function(input)
    if input == nil then
      error("Input not chosen")
      return
    else
      buffer_number = input
    end
  end)
end

local function run_code(context)
  vim.cmd.update()
  -- TODO: shell injection?!
  if buffer_number == nil then
    choose_pane()
  end

  if command == nil then
    -- assume command is given on input
    vim.system({
      "tmux",
      "send-keys",
      "-t",
      buffer_number,
      context.args,
      "C-m",
    }, {}):wait()
  else
    -- command must have been set
    vim.system({
      "tmux",
      "send-keys",
      "-t",
      buffer_number,
      command,
      "C-m",
    }, {}):wait()
  end
end

local function interactive_define_command()
  vim.ui.input({ prompt = "Command: ", completion = "shellcmd" }, function(input)
    if input == nil then
      error("Command not set")
      return
    else
      command = input
    end
  end)
end

-- set up the "plugin"
vim.api.nvim_create_user_command("VtrChoosePane", choose_pane, {})
vim.api.nvim_create_user_command("VtrRun", run_code, {
  desc = "Run command in tmux pane",
  nargs = "*",
  complete = "shellcmd",
})
vim.api.nvim_create_user_command("VtrSetCommand", interactive_define_command, {})

vim.keymap.set("n", "<leader>v", function()
  if command == nil then
    interactive_define_command()
  end

  run_code()
end, { silent = true, noremap = true })

vim.keymap.set("n", "<leader>V", function()
  interactive_define_command()
  run_code()
end, { silent = true, noremap = true })
