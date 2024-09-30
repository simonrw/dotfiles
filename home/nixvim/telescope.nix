{
  plugins.telescope = {
    enable = true;
    settings = {
      pickers = {
        git_files = {
          disable_devicons = true;
        };
        find_files = {
          disable_devicons = true;
        };
        buffers = {
          disable_devicons = true;
        };
        live_grep = {
          disable_devicons = true;
        };
        current_buffer_fuzzy_find = {
          disable_devicons = true;
        };
        lsp_definitions = {
          disable_devicons = true;
        };
        lsp_references = {
          disable_devicons = true;
        };
        diagnostics = {
          disable_devicons = true;
        };
        lsp_dynamic_workspace_symbols = {
          disable_devicons = true;
        };
      };
      defaults = {
        layout_strategy = "horizontal";
        layout_config.prompt_position = "top";
        sorting_strategy = "ascending";
      };
    };
    extensions.fzf-native = {
      enable = true;
      settings = {
        fuzzy = true;
        override_generic_sorter = true;
        override_file_sorter = true;
        case_mode = "smart_case";
      };
    };
    keymaps = {
      "<leader>f" = "git_files";
      "<leader>F" = "find_files";
      "gb" = "buffers";
      "<leader><space>" = "live_grep";
      "<leader>/" = "current_buffer_fuzzy_find";
      "gd" = "lsp_definitions";
      "gr" = "lsp_references";
      "<leader>d" = "diagnostics";
      "<leader>s" = "lsp_dynamic_workspace_symbols";
    };
  };
}
