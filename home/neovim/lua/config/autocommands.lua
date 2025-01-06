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
  }, {
  callback = function()
    do
      local binds = {}
      for _, map in ipairs(binds) do
        vim.keymap.set(map.mode, map.key, map.action, map.options)
      end
    end
  end,
  desc = "Load keymaps for LspAttach",
  event = "LspAttach",
  group = "nixvim_binds_LspAttach"
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
