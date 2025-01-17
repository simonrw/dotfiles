return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      'saghen/blink.cmp',
      {
        "folke/lazydev.nvim",
        ft = "lua", -- only load on lua files
        opts = {
          library = {
            -- See the configuration section for more details
            -- Load luvit types when the `vim.uv` word is found
            { path = "${3rd}/luv/library", words = { "vim%.uv" } }
          }
        }
      },
    },
    config = function()
      local util = require 'lspconfig.util'

      local capabilities = require("blink.cmp").get_lsp_capabilities()
      local lspServers = {
        {
          extraOptions = {
            filetypes = {
              "javascript", "javascriptreact", "javascript.jsx",
              "typescript", "typescriptreact", "typescript.tsx"
            },
            settings = {
              javascript = { format = { indentSize = 2 } },
              typescript = { format = { indentSize = 2 } }
            }
          },
          name = "ts_ls"
        }, {
        extraOptions = {
          on_attach = function(client, bufnr)
            client.config.settings.useLibraryCodeForTypes =
                false
            client.config.settings.autoSearchPaths = false
            client.config.settings
            .reportTypedDictNotRequiredAccess = "warning"
            client.config.settings.reportGeneralTypeIssues =
            "warning"
            client.config.settings.reportUnusedCallResult =
                false
            client.config.settings.reportAny = false
            client.config.settings.reportOptionalMemberAccess =
                false
            client.config.settings.reportUnknownMemberType =
                false
            client.config.settings.reportUnknownArgumentType =
                false
            client.config.settings.reportUnknownVariableType =
                false
          end
        },
        name = "pyright"
      },
        { name = "gopls" },
        { name = "gleam" },
        { name = "elmls" },
        { name = "lua_ls" },
        { name = "zls" },
        { name = "terraformls" },
        { name = "ruff" },
        {
          name = "nil_ls",
          extraOptions = {
            root_dir = util.root_pattern('shell.nix', 'flake.nix', '.git'),
          },
        },
      }

      local setup = {
        on_attach = function(client, bufnr) end,
      }

      for _, server in ipairs(lspServers) do
        if type(server) == "string" then
          require("lspconfig")[server].setup(setup)
        else
          local options = server.extraOptions

          if options == nil then
            options = setup
          else
            options = vim.tbl_extend("keep", options, setup)
          end

          require("lspconfig")[server.name].setup(options)
        end
      end

      vim.lsp.inlay_hint.enable()

      -- configure signs
      -- from: https://rsdlt.github.io/posts/rust-nvim-ide-guide-walkthrough-development-debug/
      local sign = function(opts)
        vim.fn.sign_define(opts.name,
          { texthl = opts.name, text = opts.text, numhl = '' })
      end

      sign({ name = 'DiagnosticSignError', text = '' })
      sign({ name = 'DiagnosticSignWarn', text = '' })
      sign({ name = 'DiagnosticSignHint', text = '' })
      sign({ name = 'DiagnosticSignInfo', text = '' })

      -- configure diagnostics
      -- from: https://rsdlt.github.io/posts/rust-nvim-ide-guide-walkthrough-development-debug/
      vim.diagnostic.config({
        virtual_text = false,
        signs = true,
        update_in_insert = true,
        underline = false,
        severity_sort = false,
        float = { border = 'rounded', source = true, header = '', prefix = '' }
      })

      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local buf = args.buf

          local c = vim.lsp.get_client_by_id(args.data.client_id)
          if not c then return end

          -- set up keybinds
          vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, { noremap = true, silent = true, buffer = buf, desc = "Rename symbol" })
          vim.keymap.set("n", "]d", vim.diagnostic.goto_next, { noremap = true, silent = true, buffer = buf, desc = "Go to next error" })
          vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, { noremap = true, silent = true, buffer = buf, desc = "Go to previous error" })

          -- only add these mappings if the LSP is not rust-analyzer, as these mappings are set in the rust filetype plugin
          if c.name ~= "rust-analyzer" then
            vim.keymap.set("n", "<leader>k", vim.lsp.buf.hover, { noremap = true, silent = true, buffer = buf, desc = "Get help with function signature" })
            vim.keymap.set("n", "<leader>a", function()
              vim.lsp.buf.code_action({
                source = { organizeImports = true }
              })
            end, { noremap = true, silent = true, buffer = buf, desc = "Execute code action" })
          end
        end,
      })
    end
  }
}
