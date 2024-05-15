{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.me.nixvim.oil;
in {
  options.me.nixvim.oil.enable = mkEnableOption "Oil";

  config = mkIf cfg.enable {
    programs.nixvim = {
      plugins.oil = {
        enable = true;
        settings = {
          columns = ["icon"];
          keymaps = {
            "<C-h>" = false;
          };
          view_options.show_hidden = true;
        };
      };
      keymaps = [
        {
          action = "<Cmd>Oil<CR>";
          mode = "n";
          lua = false;
          key = "-";
          options = {
            noremap = true;
            silent = true;
          };
        }
      ];
    };
  };
}
