{ lib, config, ... }:
with lib;
let
  cfg = config.me.emacs;
in
{
  options.me.emacs.enable = mkEnableOption "emacs configuration";

  config = mkIf cfg.enable {
    home.file.".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink ./emacs/init.el;
    home.file.".emacs.d/custom.el".source = config.lib.file.mkOutOfStoreSymlink ./emacs/custom.el;
  };
}
