{
  lib,
  config,
  ...
}:
with lib; {
  options = {
    me.dark-mode = mkOption {
      default = config.me.is-dark-theme;
      description = ''
        Whether to enable dark mode
      '';
    };
  };
}
