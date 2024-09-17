{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.context;
in {
  options.me.nixvim.context = {
    enable = mkEnableOption "context";
    method = mkOption {
      type = types.enum [
        "treesitter-context"
        "barbecue"
      ];
      default = "barbecue";
    };
  };

  config = mkIf cfg.enable {
    plugins.treesitter-context = mkIf (cfg.method == "treesitter-context") {
      enable = true;
      settings.max_lines = 3;
    };
    plugins.barbecue = mkIf (cfg.method == "barbecue") {
      enable = true;
    };
  };
}
