{ lib, config, ... }:
with lib;
let
  cfg = config.me.nixvim.completion.copilot;
in
{ 
  options.me.nixvim.completion.copilot = mkEnableOption "Copilot completion";

  config = mkIf cfg {
    plugins.copilot-cmp.enable = true;
    plugins.copilot-lua = {
      enable = true;
      suggestion.enabled = false;
      panel.enabled = false;
    };

    plugins.cmp.settings.sources = [
      { name = "copilot"; priority = 400; }
    ];
  };
}
