local lualine = require('lualine')

local function lsp()
    if #vim.lsp.buf_get_clients() > 0 then
        return require('lsp-status').status()
    end

    return ""

    -- local messages = require('lsp-status').messages()
    -- -- just take the first for now
    -- if messages[1] ~= nil then
    --     return messages[1].title or ""
    -- end


end

local lualine_c
if vim.g.completion_framework == 'nvim' then
    lualine_c = {
            { 'filename',
              file_status = false,
              path = 1,
            }, lsp}
else
    lualine_c = {
            { 'filename',
              file_status = false,
              path = 1,
            }}
end

lualine.setup({
    options = {
        icons_enabled = true,
        theme = 'auto',
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
        disabled_filetypes = {},
        always_divide_middle = true,
        globalstatus = false,
        refresh = {
            statusline = 100,
        },
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'diff'},
        lualine_c = lualine_c,
        lualine_x = {'encoding', 'filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {}
    },
    tabline = {},
    extensions = { 'quickfix', 'fugitive' }
})

