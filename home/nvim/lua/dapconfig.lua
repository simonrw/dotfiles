local dapui = require('dapui')
dapui.setup()
local dap = require("dap")

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
  dapui.close()
end

-- bindings
local function add_dap_mapping(key, cb, desc)
    vim.api.nvim_set_keymap('n', key, '', {
        noremap = true,
        callback = cb,
        desc = desc,
    })
end

add_dap_mapping(
    '<leader>db',
    function()
        dap.toggle_breakpoint()
    end,
    "Toggle breakpoint"
)

add_dap_mapping(
    '<leader>dc',
    function()
        dap.continue()
    end,
    "Continue/start execution"
)

add_dap_mapping(
    '<leader>di',
    function()
        dap.step_into()
    end,
    "Step into frame"
)

add_dap_mapping(
    '<leader>dn',
    function()
        dap.step_over()
    end,
    "Step over next"
)

add_dap_mapping(
    '<leader>do',
    function()
        dap.step_out()
    end,
    "Step out of frame"
)

add_dap_mapping(
    '<leader>dt',
    function()
        dap.terminate()
    end,
    "Stop debugging"
)
