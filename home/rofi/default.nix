{config, ...}: let
  inherit (config.lib.formats.rasi) mkLiteral;
in {
  config = {
    programs.rofi = {
      enable = true;
      theme = ./nord.rasi;
      extraConfig = {
        line-margin = 10;
        show-icons = false;
      };
    };
    services.clipmenu = {
      enable = true;
      launcher = "rofi";
    };
  };
}
