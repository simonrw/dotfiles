return {
  {
    'mfussenegger/nvim-dap-python',
    dependencies = {
      {
        'mfussenegger/nvim-dap',
        config = function()
          local opts = function(desc)
            return { noremap = true, silent = true, desc = desc }
          end

          vim.keymap.set("n", "<leader>bb", function() require("dap").toggle_breakpoint() end, opts("Toggle breakpoint"))
          vim.keymap.set("n", "<leader>br", function() require("dap").repl.toggle() end, opts("Toggle repl"))
          vim.keymap.set("n", "<leader>bl", function() require("dap").run_last() end, opts("Run last debugging session"))
          vim.keymap.set("n", "<leader>bf", function()
            local widgets = require('dap.ui.widgets')
            widgets.centered_float(widgets.frames)
          end, opts("Show frames"))
          vim.keymap.set("n", "<leader>bs", function()
            local widgets = require('dap.ui.widgets')
            widgets.centered_float(widgets.scopes)
          end, opts("Show scopes"))

          -- map pycharm bindings
          vim.keymap.set({ "i", "n" }, "<F7>", function() require("dap").step_into() end, opts("Step into"))
          vim.keymap.set({ "i", "n" }, "<F8>", function() require("dap").step_over() end, opts("Step over"))
          vim.keymap.set({ "i", "n" }, "<S-F8>", function() require("dap").step_out() end, opts("Step out"))
          vim.keymap.set({ "i", "n" }, "<F9>", function() require("dap").continue() end, opts("Continue"))

          require("dap").adapters.codelldb = {
            type = "server",
            port = "${port}",
            executable = {
              command = "codelldb",
              args = { "--port", "${port}" },
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
