{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfg = config.me.nixvim.render-markdown;
in {
  options.me.nixvim.render-markdown.enable = mkEnableOption "render-markdown";

  config = mkIf cfg.enable {
    extraPlugins = with pkgs.vimPlugins; [
      render-markdown-nvim
    ];
  };
}
