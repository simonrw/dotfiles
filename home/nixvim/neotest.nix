{
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
    programs.nixvim = {
      plugins.neotest = {
        enable = true;
        adapters = {
          go.enable = true;
          python.enable = true;
          rust.enable = true;
        };
      };
      keymaps = [
        (keymap {
          key = "tf";
          action = ''require("neotest").run.run(vim.fn.expand("%"))'';
        })
        (keymap {
          key = "tl";
          action = ''require("neotest").run.run_last()'';
        })
        (keymap {
          key = "tn";
          action = ''require("neotest").run.run()'';
        })
      ];
    };
  };
}
