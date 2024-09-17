{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.which-key;
in {
  options.me.nixvim.which-key.enable = mkEnableOption "which-key";

  config = mkIf cfg.enable {
    plugins.which-key = {
      enable = true;
    };
  };
}
