{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.neogit;
in {
  options.me.nixvim.neogit.enable = mkEnableOption "neogit";

  config = mkIf cfg.enable {
    plugins.neogit = {
      enable = true;
    };

    assertions = [
      {
        assertion = !config.me.nixvim.fugitive.enable;
        message = "Fugitive and neogit are mutually exclusive";
      }
    ];
  };
}
