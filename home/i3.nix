{
  pkgs,
  config,
  system,
  lib,
  ...
}:
with lib; let
  cfg = config.me.wm.i3;

  mod = "Mod4";

  focus-last = pkgs.stdenv.mkDerivation {
    name = "focus-last";
    propagatedBuildInputs = with pkgs; [
      (python3.withPackages (ps:
        with ps; [
          i3ipc
        ]))
    ];
    dontUnpack = true;
    installPhase = ''
      install -Dm755 ${./i3/focus-last.py} $out/bin/i3-focus-last
    '';
  };

  browser-command =
    {
      # fallback for linux vm on my mba
      "aarch64-linux" = "exec firefox";
    }
    .${system}
    or "exec ${config.me.defaults.browser}";
in {
  options.me.wm.i3 = {
    enable = mkEnableOption "i3 customisations";
  };

  config.xsession.windowManager.i3 = mkIf cfg.enable {
    enable = true;
    config = {
      modifier = mod;
      startup = let
        execAlways = cmd: {
          command = cmd;
          always = true;
          notification = false;
        };
      in [
        (execAlways ''${pkgs.hsetroot}/bin/hsetroot -solid "#4f535c"'')
        (execAlways ''${focus-last}/bin/i3-focus-last'')
        (execAlways ''blueman-applet'')
      ];
      menu = "${pkgs.rofi}/bin/rofi -show drun";
      window = {
        titlebar = false;
        border = 3;
      };
      colors = {
        focused = {
          border = "#4c7899";
          background = "#285577";
          text = "#ffffff";
          indicator = "#88c0d0";
          childBorder = "#5e81ac";
        };
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

        # toggle sticky windows
        "${mod}+Shift+s" = "sticky toggle";

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

        # switch between windows
        "${mod}+Tab" = "exec ${focus-last}/bin/i3-focus-last --switch";

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

        "${mod}+c" = browser-command;
        "${mod}+Return" = "exec alacritty";
        "${mod}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";
        "${mod}+space" = "exec ${pkgs.rofi}/bin/rofi -show drun";

        # handle my moonlander keybindings
        "Mod1+Mod4+t" = "exec alacritty";
        "Mod1+Mod4+c" = browser-command;
        "Mod1+Mod4+e" = "exec obsidian";
        "Mod1+Mod4+s" = "exec slack";

        # media keys
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioPause" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";

        # volume control
        "XF86AudioRaiseVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -i 5";
        "XF86AudioLowerVolume" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -d 5";
        "XF86AudioMute" = "exec --no-startup-id ${pkgs.pamixer}/bin/pamixer -t";

        # modes
        "${mod}+r" = "mode resize";
      };
      modes = {
        resize = {
          "h" = "resize shrink width 10px or 10ppt";
          "j" = "resize grow height 10 px or 10 ppt";
          "k" = "resize shrink height 10 px or 10 ppt";
          "l" = "resize grow width 10 px or 10 ppt";

          "Return" = "mode default";
          "Escape" = "mode default";
        };
      };
      gaps = {
        inner = 4;
        outer = 4;
      };
    };
  };
}
