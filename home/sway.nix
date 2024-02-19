{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.wm.sway;

  mod = "Mod1";
in {
  options.me.wm.sway = {
    enable = mkEnableOption "Sway customisation";
  };
  config.wayland.windowManager.sway = mkIf cfg.enable {
    enable = true;
    config = rec {
      modifier = mod;
      terminal = "kitty";
      gaps = {
        inner = 8;
        outer = 4;
      };
      bars = [
        {
          position = "top";
          fonts = {
            # TODO: set this in font.nix
            names = [config.me.font-name];
          };
        }
      ];
      keybindings = {
        "${mod}+Shift+Q" = "kill";
        # change focus
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";
        # alternatively, you can use the cursor keys:
        "${mod}+Left" = "focus left";
        "${mod}+Down" = "focus down";
        "${mod}+Up" = "focus up";
        "${mod}+Right" = "focus right";

        # move focused window
        "${mod}+Shift+H" = "move left";
        "${mod}+Shift+J" = "move down";
        "${mod}+Shift+K" = "move up";
        "${mod}+Shift+L" = "move right";

        # alternatively, you can use the cursor keys:
        "${mod}+Shift+Left" = "move left";
        "${mod}+Shift+Down" = "move down";
        "${mod}+Shift+Up" = "move up";
        "${mod}+Shift+Right" = "move right";

        # split in horizontal orientation
        "${mod}+Shift+backslash" = "split h";

        # split in vertical orientation
        "${mod}+underscore" = "split v";

        # enter fullscreen mode for the focused container
        "${mod}+Shift+f" = "fullscreen";

        # change container layout (stacked, tabbed, default)
        "${mod}+s" = "layout stacking";
        "${mod}+w" = "layout tabbed";
        "${mod}+e" = "layout default";

        # toggle tiling / floating
        "${mod}+Shift+space" = "floating toggle";

        # change focus between tiling / floating windows
        "${mod}+space" = "focus mode_toggle";

        # focus the parent container
        "${mod}+a" = "focus parent";
        # switch to workspace
        "${mod}+1" = "workspace number 1";
        "${mod}+2" = "workspace number 2";
        "${mod}+3" = "workspace number 3";
        "${mod}+4" = "workspace number 4";
        "${mod}+5" = "workspace number 5";
        "${mod}+6" = "workspace number 6";
        "${mod}+7" = "workspace number 7";
        "${mod}+8" = "workspace number 8";
        "${mod}+9" = "workspace number 9";
        "${mod}+0" = "workspace number 10";
        "${mod}+n" = "workspace next";
        "${mod}+p" = "workspace prev";

        # move focused container to workspace
        "${mod}+Shift+exclam" = "move container to workspace 1";
        "${mod}+Shift+quotedbl" = "move container to workspace 2";
        "${mod}+Shift+sterling" = "move container to workspace 3";
        "${mod}+Shift+dollar" = "move container to workspace 4";
        "${mod}+Shift+percent" = "move container to workspace 5";
        "${mod}+Shift+asciicircum" = "move container to workspace 6";
        "${mod}+Shift+ampersand" = "move container to workspace 7";
        "${mod}+Shift+asterisk" = "move container to workspace 8";
        "${mod}+Shift+parenleft" = "move container to workspace 9";
        "${mod}+Shift+parenright" = "move container to workspace 10";

        # reload the configuration file
        "${mod}+Shift+C" = "reload";
        # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
        "${mod}+Shift+R" = "restart";
        # exit i3 (logs you out of your X session)
        "${mod}+Ctrl+Shift+E" = "exit";

        "${mod}+c" = "exec ${config.me.defaults.browser.command or config.me.defaults.browser}";
        "${mod}+Return" = "exec ${pkgs.${config.me.defaults.terminal}}/bin/${config.me.defaults.terminal}";
        "${mod}+d" = "exec ${pkgs.wofi}/bin/wofi -show drun";
      };
    };
  };
}
