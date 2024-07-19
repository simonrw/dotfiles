{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.me.nixvim.cody;
in {
  options.me.nixvim.cody.enable = mkEnableOption "cody";

  config = mkIf cfg.enable {
    programs.nixvim = {
      extraPlugins = with pkgs.vimPlugins; [
        sg-nvim
      ];
      extraConfigLua = ''
        require('sg').setup()
      '';
      plugins.cmp.settings.sources = lib.mkBefore [
        {name = "cody";}
      ];
    };
  };
}
