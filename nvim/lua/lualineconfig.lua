local lualine = require('lualine')
local lsp_status = require('lsp-status')
local lspconfig = require('lspconfig')

local function lsp()
    local messages = require('lsp-status').messages()
    -- just take the first for now
    if messages[1] ~= nil then
        return messages[1].title or ""
    end
    return ""
end

local function obsession_status()
    return vim.api.nvim_eval([[ObsessionStatus('$')]])
end

lualine.setup({
    options = {
        icons_enabled = true,
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
        lualine_x = {obsession_status, 'encoding', 'filetype'},
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
