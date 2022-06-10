nnoremap('<leader>b', [[:lua require('dap').toggle_breakpoint()<cr>]])
nnoremap('<leader>Bc', [[:lua require('dap').continue()<cr>]])
nnoremap('<leader>Bo', [[:lua require('dap').step_over()<cr>]])
nnoremap('<leader>Bi', [[:lua require('dap').step_into()<cr>]])
nnoremap('<leader>Bb', [[:lua require('dap').step_out()<cr>]])

local dap = require('dap')

-- set up adapters
require('dap-go').setup()

dap.adapters.lldb = {
    type = 'executable',
    command = '/usr/local/opt/llvm/bin/lldb-vscode',
    name = 'lldb',
}

dap.configurations.cpp = {
    {
        name = 'Launch',
        type = 'lldb',
        request = 'launch',
        program = function()
            return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = true,
        args = {},
    },
}
dap.configurations.rust = dap.configurations.cpp

require('dap-python').setup('~/.local/virtualenvs/debugpy/bin/python')

-- set up dap ui
local dap, dapui = require("dap"), require("dapui")
dapui.setup()
dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end

