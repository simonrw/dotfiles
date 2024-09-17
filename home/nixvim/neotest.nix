{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  keymap = {
    key,
    action,
    mode ? "n",
    lua ? true,
  }: {
    inherit action mode key lua;
    options = {
      noremap = true;
      silent = true;
    };
  };
  cfg = config.me.nixvim.neotest;
in {
  options.me.nixvim.neotest.enable = mkEnableOption "neotest";
  config = mkIf cfg.enable {
    plugins.neotest = {
      enable = true;
      adapters = {
        go.enable = true;
        python.enable = true;
        rust.enable = true;
      };
      settings = {
        output_panel.open = "tabnew";
        output.enabled = false;
      };
    };
    globals = {
      cursorhold_updatetime = 100;
    };
    extraPlugins = with pkgs.vimPlugins; [
      FixCursorHold-nvim
    ];
    keymaps = [
      (keymap {
        key = "tf";
        action = ''function() require("neotest").run.run(vim.fn.expand("%")) end'';
      })
      (keymap {
        key = "tl";
        action = ''function() require("neotest").run.run_last() end'';
      })
      (keymap {
        key = "tn";
        action = ''function() require("neotest").run.run() end'';
      })
      (keymap {
        key = "to";
        action = ''function() require("neotest").output_panel.toggle() end'';
      })
    ];
  };
}
