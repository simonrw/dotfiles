local o = vim.opt_local

local autocommands = {
  -- {command = "setlocal wrap conceallevel=2 concealcursor=n", event = {"FileType"}, pattern = {"markdown"}},
  {
    event = { "FileType" },
    pattern = { "markdown" },
    callback = function()
      o.conceallevel = 2
      o.concealcursor = "n"
      o.wrap = true
    end,
  },
  {
    callback = function()
      o.shiftwidth = 2
      o.tabstop = 2
    end,
    event = { "FileType" },
    pattern = { "hcl", "terraform" }
  }, {
  callback = function()
    o.shiftwidth = 2
    o.tabstop = 2
  end,
  event = { "FileType" },
  pattern = { "javascript", "typescript" }
}, {
  callback = function()
    require("vim.highlight").on_yank()
  end,
  event = { "TextYankPost" },
  group = "lua-highlight"
},
  {
    event = { "TermOpen" },
    group = "terminal-settings",
    callback = function()
      -- enter insert mode only if we open a terminal and switch to it
      vim.defer_fn(function()
        if vim.api.nvim_get_option_value('buftype', { buf = 0 }) == 'terminal' then
          vim.cmd([[startinsert]])
        end
      end, 100)
    end
  },
  {
    command = "if &diff == 1 | diffupdate | endif",
    event = { "BufWritePost" },
    group = "diff-mode"
  }
}

for _, autocmd in ipairs(autocommands) do
  vim.api.nvim_create_autocmd(autocmd.event, {
    group = autocmd.group,
    pattern = autocmd.pattern,
    buffer = autocmd.buffer,
    desc = autocmd.desc,
    callback = autocmd.callback,
    command = autocmd.command,
    once = autocmd.once,
    nested = autocmd.nested
  })
end

-- open file at last opened position
vim.api.nvim_create_autocmd('BufReadPost', {
  pattern = { '*' },
  desc = 'When editing a file, always jump to the last known cursor position',
  callback = function()
    local line = vim.fn.line '\'"'
    if
        line >= 1
        and line <= vim.fn.line '$'
        and (vim.bo.filetype ~= 'commit') and (vim.bo.filetype ~= "gitcommit")
        and vim.fn.index({ 'xxd', 'gitrebase' }, vim.bo.filetype) == -1
    then
      vim.cmd 'normal! g`"'
    end
  end,
})

-- templates
vim.api.nvim_create_autocmd('BufNewFile', {
  group = vim.api.nvim_create_augroup('templates', { clear = true }),
  desc = "Load template on new file",
  callback = function()
    local home = os.getenv("HOME")
    local ftype = vim.bo.filetype
    local tpl_path = home .. "/.config/nvim/templates/" .. ftype .. ".tpl"
    if not vim.uv.fs_stat(tpl_path) then
      return
    end
    local f = io.open(tpl_path, "r")
    if not f then
      return
    end
    local content = f:read("*a")
    vim.snippet.expand(content)
  end,
})
