{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.me.nixvim.completion;
in {
  options.me.nixvim.completion = {
    enable = mkEnableOption "Completion";
    emoji = mkEnableOption "Emoji completion";
    supermaven = mkEnableOption "Supermaven";
    require-trigger = mkOption {
      type = types.bool;
      default = false;
      description = "Require keypress to enable";
    };
  };
  config = mkIf cfg.enable {
    plugins = {
      cmp-emoji.enable = cfg.emoji;
      cmp-nvim-lsp-signature-help.enable = true;
      cmp = {
        enable = true;
        lazyLoad.settings = {
          event = "InsertEnter";
        };
        settings = {
          autoEnableSources = true;
          preselect = "None";
          completion = {
            completeopt = "menu,menuone,noinsert,noselect";
            autocomplete =
              if cfg.require-trigger
              then false
              else null;
          };
          sources =
            [
              {name = "nvim_lsp_signature_help"; priority = 1000; groupIndex = 1;}
              {name = "nvim_lsp"; keyword_length = 2; groupIndex = 1;}
              {name = "treesitter"; keyword_length = 2; groupIndex = 1;}
              {name = "path"; groupIndex = 1;}
              {
                name = "buffer";
                # Words from other open buffers can also be suggested.
                option.get_bufnrs.__raw = "vim.api.nvim_list_bufs";
                groupIndex = 1;
              }
            ]
            ++ (lib.optionals cfg.emoji [
              {name = "emoji";}
            ]);
          mapping = {
            "<C-p>" = "cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert })";
            "<C-n>" = "cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert })";
            "<C-y>" = "cmp.mapping.confirm({ select = true, behavior = cmp.ConfirmBehavior.Insert })";
            "<C-Space>" = "cmp.mapping.complete()";
            "<C-e>" = "cmp.config.disable";
          };
          snippet.expand = ''
            function(args)
              vim.snippet.expand(args.body)
            end
          '';
        };
      };
    };
    extraPlugins = mkIf cfg.supermaven [
      pkgs.vimPlugins.supermaven-nvim
    ];
    extraConfigLua = mkIf cfg.supermaven ''
      require('supermaven-nvim').setup({})
    '';
  };
}
