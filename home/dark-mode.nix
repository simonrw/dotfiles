{ lib, ... }:
with lib;
{
  options = {
    dark-mode = mkOption {
      default = true;
      description = ''
        Whether to enable dark mode
      '';
    };
  };
}
