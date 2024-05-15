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
  };

  config = mkIf cfg.enable {
    programs.nixvim.plugins.treesitter-context = {
      enable = true;
      maxLines = 3;
    };
  };
}
