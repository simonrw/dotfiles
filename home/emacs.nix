{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.emacs;
in {
  options.me.emacs.enable = mkEnableOption "emacs configuration";

  config = mkIf cfg.enable {
    home.file.".emacs.d/init.el".source = ./emacs/init.el;
    home.file.".emacs.d/custom.el".source = ./emacs/custom.el;
  };
}
