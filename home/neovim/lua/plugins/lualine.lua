local function diagnostic(level)
  if (vim.diagnostic.count(0)[level] or 0) > 0 then
    return "x"
  else
    return ""
  end
end

local function error_ind() return diagnostic(vim.diagnostic.severity.ERROR) end
local function warn_ind() return diagnostic(vim.diagnostic.severity.WARN) end
local function info_ind() return diagnostic(vim.diagnostic.severity.INFO) end
local function note_ind() return diagnostic(vim.diagnostic.severity.HINT) end

return {
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = {
      options = {
        icons_enabled = false,
        component_separators = "",
        section_separators = "",
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = { "filename" },
        lualine_c = {
          {
            error_ind,
            color = { fg = "#8aadf4" },
          },
        },
        lualine_x = {},
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
    },
  }
}
