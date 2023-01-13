{ config, ... }:
{
  xdg = {
    configFile."kitty/kitty.conf" = {
      source = ./kitty/kitty.conf;
    };
    configFile."kitty/themes/theme.conf" =
      let
        theme =
          if config.dark-mode
          then "srw.conf"
          else "papercolor-light.conf";

      in
      {
        source = ./kitty/themes/${theme};
      };
  };
}
