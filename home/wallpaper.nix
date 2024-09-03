{
  lib,
  isDarwin,
  config,
  ...
}:
with lib; let
  cfg = config.me.wallpaper;
in {
  options.me.wallpaper = {
    enable = mkEnableOption "Custom wallpaper";
    path = mkOption {
      type = types.path;
      description = "Path to a wallpaper";
    };
  };

  config = mkIf (isDarwin && cfg.enable) {
    # activate the wallpaper
    home.activation.setWallpaper = hm.dag.entryAfter ["WriteBoundary"] ''
      osascript -e "tell application \"System Events\" to tell every desktop to set picture to \"${cfg.wallpaper}\" as POSIX file"
    '';
  };
}
