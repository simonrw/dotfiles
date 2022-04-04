local lualine = require('lualine')
local lsp_status = require('lsp-status')
local lspconfig = require('lspconfig')

local function lsp()
    local messages = require('lsp-status').messages()
    -- just take the first for now
    if messages[1] ~= nil then
        if messages[1].message ~= nil then
            return messages[1].title
        end
    end
    return ""
end

lualine.setup({
    options = {
        icons_enabled = false,
        theme = 'auto',
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
        disabled_filetypes = {},
        always_divide_middle = true,
        globalstatus = true,
    },
    sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'diff', 'diagnostics'},
        lualine_c = {'filename', lsp},
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
    extensions = {}
})
