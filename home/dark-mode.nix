{ lib, ... }:
with lib;
{
  options = {
    me.dark-mode = mkOption {
      default = true;
      description = ''
        Whether to enable dark mode
      '';
    };
  };
}
