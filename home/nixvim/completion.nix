{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.completion;
in {
  options.me.nixvim.completion = {
    enable = mkEnableOption "Completion";
    require-trigger = mkOption {
      type = types.bool;
      default = false;
      description = "Require keypress to enable";
    };
  };
  config.programs.nixvim = mkIf cfg.enable {
    plugins = {
      cmp_luasnip.enable = true;
      cmp = {
        enable = true;
        settings = {
          autoEnableSources = true;
          preselect = "None";
          completion = {
            completeopt = "menu,menuone,noinsert,noselect";
            autocomplete = !cfg.require-trigger;
          };
          sources = [
            {name = "nvim_lsp";}
            {name = "path";}
            {name = "buffer";}
            {name = "luasnip";}
          ];
          mapping = {
            "<C-p>" = "cmp.mapping.select_prev_item({ select = true })";
            "<C-n>" = "cmp.mapping.select_next_item({ select = true })";
            "<C-y>" = "cmp.mapping.confirm({ select = true })";
            "<C-Space>" = "cmp.mapping.complete()";
            "<C-e>" = "cmp.config.disable";
          };
          snippet.expand = ''
            function(args)
              require('luasnip').lsp_expand(args.body)
            end
          '';
        };
      };
      luasnip = {
        enable = true;
        extraConfig = {
          history = true;
          updateevents = "TextChanged,TextChangedI";
          enable_autosnippets = true;
        };
      };
    };
  };
}
