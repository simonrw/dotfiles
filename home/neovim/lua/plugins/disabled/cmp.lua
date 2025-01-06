return {
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "neovim/nvim-lspconfig",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-emoji",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp-signature-help",
    },
    config = function()
      local cmp = require("cmp")

      cmp.setup({
        autoEnableSources = true,
        completion = {
          completeopt = "menu.menuone,noinsert,noselect",
        },
        mapping = {
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.config.disable,
          ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-y>"] = cmp.mapping.confirm({ select = true, behavior = cmp.ConfirmBehavior.Insert }),
        },
        snippet = {
          expand = function(args) vim.snippet.expand(args.body) end,
        },
        sources = cmp.config.sources({
          { name = 'nvim_lsp_signature_help' },
          { name = "nvim_lsp" },
          { name = "buffer" },
        }, {
          { name = "emoji" },
        }),
      })
    end,
  },
}
