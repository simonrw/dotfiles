{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.nixvim.fugitive;

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
  options.me.nixvim.fugitive = {
    enable = mkEnableOption "fugitive";
  };

  config = mkIf cfg.enable {
    plugins.fugitive.enable = true;
    autoCmd = [
      # support closing fugitive window with 'q'
      {
        event = ["FileType"];
        pattern = ["fugitive"];
        command = "nmap <buffer> q gq";
      }
    ];
    keymaps = [
      (keymap {
        key = "<leader>gc";
        action = ":Git commit -v<cr>";
      })
      (keymap {
        key = "<leader>gd";
        action = ":Gvdiff<cr>";
      })
      (keymap {
        key = "<leader>gw";
        action = ":Gwrite<cr>";
      })
      (keymap {
        key = "<leader>gr";
        action = ":Gread<cr>";
      })
      (keymap {
        key = "gs";
        action = ":Git<cr>";
      })
      (keymap {
        key = "<leader>ga";
        action = ":Git commit -v --amend<cr>";
      })
    ];

    assertions = [
      {
        assertion = !config.me.nixvim.neogit.enable;
        message = "Fugitive and neogit are mutually exclusive";
      }
    ];
  };
}
