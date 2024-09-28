{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.neogit;
    
  keymap = {
    key,
    action,
    mode ? "n",
    lua ? false,
  }: {
    inherit action mode key lua;
    options = {
      noremap = true;
      silent = true;
    };
  };
in {
  options.me.nixvim.neogit.enable = mkEnableOption "neogit";

  config = mkIf cfg.enable {
    plugins.neogit = {
      enable = true;
    };

    keymaps = [
      (keymap {
        key = "gs";
        action = ":Neogit<cr>";
      })
      (keymap {
        key = "<leader>gc";
        action = ":Neogit commit<cr>";
      })
    ];

    assertions = [
      {
        assertion = !config.me.nixvim.fugitive.enable;
        message = "Fugitive and neogit are mutually exclusive";
      }
    ];
  };
}
