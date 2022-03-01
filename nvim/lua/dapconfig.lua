nnoremap('<leader>b', [[:lua require('dap').toggle_breakpoint()<cr>]])
nnoremap('<F5>', [[:lua require('dap').continue()<cr>]])
nnoremap('<F10>', [[:lua require('dap').step_over()<cr>]])
nnoremap('<F11>', [[:lua require('dap').step_into()<cr>]])
nnoremap('<F12>', [[:lua require('dap').step_out()<cr>]])

-- set up adapters
require('dap-go').setup()

-- set up dap ui
require('dapui').setup()
