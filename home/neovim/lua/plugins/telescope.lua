return {
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.8',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    },
    config = function()
      require("telescope").setup({
        defaults = {
          layout_config = { prompt_position = "top" },
          layout_strategy = "horizontal",
          sorting_strategy = "ascending"
        },
        extensions = {
          fzf = {
            case_mode = "smart_case",
            fuzzy = true,
            override_file_sorter = true,
            override_generic_sorter = true
          }
        },
        pickers = {
          buffers = { disable_devicons = true },
          current_buffer_fuzzy_find = { disable_devicons = true },
          diagnostics = { disable_devicons = true },
          find_files = { disable_devicons = true },
          git_files = { disable_devicons = true },
          live_grep = { disable_devicons = true },
          lsp_definitions = { disable_devicons = true },
          lsp_dynamic_workspace_symbols = { disable_devicons = true },
          lsp_references = { disable_devicons = true }
        }
      })

      -- set up keymaps
      vim.keymap.set("n", "<leader>/", function() require("telescope.builtin").current_buffer_fuzzy_find() end,
        { silent = true, noremap = true, desc = "Find in the current buffer" })
      vim.keymap.set("n", "<leader><space>", function() require("telescope.builtin").live_grep() end,
        { silent = true, noremap = true, desc = "Perform a live grep over the project" })
      vim.keymap.set("n", "<leader>F", function() require("telescope.builtin").find_files() end,
        { silent = true, noremap = true, desc = "Find any file" })
      vim.keymap.set("n", "<leader>d", function() require("telescope.builtin").diagnostics() end,
        { silent = true, noremap = true, desc = "LSP diagnostics" })
      vim.keymap.set("n", "<leader>f", function() require("telescope.builtin").git_files() end,
        { silent = true, noremap = true, desc = "Find git files" })
      vim.keymap.set("n", "<leader>S", function() require("telescope.builtin").lsp_dynamic_workspace_symbols() end,
        { silent = true, noremap = true, desc = "LSP workspace symbols" })
      vim.keymap.set("n", "gb", function() require("telescope.builtin").buffers() end,
        { silent = true, noremap = true, desc = "Switch to buffer" })
      vim.keymap.set("n", "gd", function() require("telescope.builtin").lsp_definitions() end,
        { silent = true, noremap = true, desc = "LSP definitions" })
      vim.keymap.set("n", "gr", function() require("telescope.builtin").lsp_references() end,
        { silent = true, noremap = true, desc = "LSP references" })
      vim.keymap.set("n", "gi", function() require("telescope.builtin").lsp_implementations() end,
        { silent = true, noremap = true, desc = "LSP implementations" })
      vim.keymap.set("n", "<leader>s", function() require("telescope.builtin").lsp_document_symbols() end,
        { silent = true, noremap = true, desc = "LSP symbols in the current buffer" })
      vim.keymap.set("n", "<leader>j", function() require("telescope.builtin").jumplist({ show_line = false, }) end,
        { silent = true, noremap = true, desc = "Choose jump location" })
      vim.keymap.set("n", "<leader>ht", function() require("telescope.builtin").help_tags() end,
        { silent = true, noremap = true, desc = "Neovim help tags" })
    end
  }
}
