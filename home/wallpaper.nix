{lib, ...}:
with lib; {
  options.me.wallpaper = mkOption {
    type = types.path;
    description = "Path to a wallpaper";
  };
}
