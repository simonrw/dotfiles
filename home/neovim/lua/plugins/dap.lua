return {
  {
    'mfussenegger/nvim-dap-python',
    dependencies = {
      {
        'mfussenegger/nvim-dap',
        config = function()
          local opts = { noremap = true, silent = true }

          vim.keymap.set("n", "<leader>b", function() require("dap").toggle_breakpoint() end, opts)

          -- map pycharm bindings
          vim.keymap.set({ "i", "n" }, "<F7>", function() require("dap").step_into() end, opts)
          vim.keymap.set({ "i", "n" }, "<F8>", function() require("dap").step_over() end, opts)
          vim.keymap.set({ "i", "n" }, "<S-F8>", function() require("dap").step_out() end, opts)
          vim.keymap.set({ "i", "n" }, "<F9>", function() require("dap").continue() end, opts)

          require("dap").adapters.codelldb = {
            type = "server",
            port = "${port}",
            executable = {
              command = "codelldb",
              args = {"--port", "${port}"},
            },
          }
        end,
      },
    },
    config = function()
      require("dap-python").setup("python")
    end,
  },
}
