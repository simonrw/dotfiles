-- Plugin to add a `TodoItems` command to markdown buffers, which greps for items like
-- - [ ] Do the thing #TODO

local function find_todo_items()
  vim.cmd([[silent! vimgrep '\v[-*]\s+\[\s+\].+(#TODO)' %]])
  vim.cmd.copen()
end

local function find_learnings()
  vim.cmd([[silent! vimgrep '\v.+(#learnings)' %]])
  vim.cmd.copen()
end

local function find_matching(input)
  local match_type = input.fargs[1]
  if match_type == "todo" or match_type == nil then
    return find_todo_items()
  elseif match_type == "learnings" then
    return find_learnings()
  end
end

local commands = {"todo", "learnings"}

vim.api.nvim_create_autocmd({ "FileType" }, {
  group = vim.api.nvim_create_augroup("Notes", {}),
  callback = function()
    vim.api.nvim_buf_create_user_command(0, "NotesFind", find_matching, {
      nargs = "?",
      complete = function(_, line)
        local l = vim.split(line, "%s+")
        return vim.tbl_filter(function(val)
          return vim.startswith(val, l[2])
        end, commands)
      end,
    })
  end,
})
