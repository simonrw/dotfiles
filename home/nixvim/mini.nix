{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.mini;
in {
  options.me.nixvim.mini = {
    enable = mkEnableOption "mini.nvim";
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.mini = {
      enable = true;
      modules = {
        # Note: not actually AI!
        ai = {
          custom_textobjects = {
            f.__raw = ''require("mini.ai").gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" })'';
            c.__raw = ''require("mini.ai").gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" })'';
          };
        };
      };
    };
  };
}
