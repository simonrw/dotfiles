{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.zed;
in {
  options.me.zed.enable = mkEnableOption "zed configuration";

  config = mkIf cfg.enable {
    # not xdg as this path is fixed even on macos
    home.file.".config/zed" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix-config/home/zed";
      recursive = true;
    };
  };
}

