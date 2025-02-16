return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      'nvim-treesitter/nvim-treesitter-textobjects'
    },
    build = ":TSUpdate",
    config = function()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
        ensure_installed = { "lua", "vim", "rust", "python", "nix", "yaml", "hcl", "terraform", "typescript", "javascript" },
        sync_install = false,
        highlight = { enable = true, additional_vim_regex_highlighting = false },
        indent = { enable = false },
        -- TODO: will need to get rid of this with v1.0
        incremental_selection = {
          enable = true,
          keymaps = {
            node_incremental = "v",
            node_decremental = "V",
          },
        },
        textobjects = {
          select = {
            enable = true,
            keymaps = {
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
            },
            selection_modes = {
              ['@function.outer'] = 'V',
              ['@class.outer'] = 'V',
            },
          }
        },
      })

      -- configure folding
      vim.wo.foldmethod = "expr"
      vim.wo.foldexpr = "nvim_treesitter#foldexpr()"
      vim.opt.foldlevel = 99
    end
  },
}
