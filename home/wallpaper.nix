{
  lib,
  isDarwin,
  config,
  ...
}:
with lib; let
  wallpaper = config.me.wallpaper;
in {
  options.me.wallpaper = mkOption {
    type = types.path;
    description = "Path to a wallpaper";
  };

  config = mkIf isDarwin {
    # activate the wallpaper
    home.activation.setWallpaper = hm.dag.entryAfter ["WriteBoundary"] ''
      osascript -e "tell application \"System Events\" to tell every desktop to set picture to \"${wallpaper}\" as POSIX file"
    '';
  };
}
