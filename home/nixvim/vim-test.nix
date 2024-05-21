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
    lua ? false,
  }: {
    inherit action mode key lua;
    options = {
      noremap = true;
      silent = true;
    };
  };

  cfg = config.me.nixvim.vim-test;
in {
  options.me.nixvim.vim-test = {
    enable = mkEnableOption "vim-test";
  };

  config = mkIf cfg.enable {
    programs.nixvim = {
      extraPlugins = with pkgs.vimPlugins; [
        vim-test
      ];
      keymaps = [
        (keymap {
          key = "tf";
          action = ":update|:TestFile<cr>";
        })
        (keymap {
          key = "tl";
          action = ":update|:TestLast<cr>";
        })
        (keymap {
          key = "tn";
          action = ":update|:TestNearest<cr>";
        })
        (keymap {
          key = "ta";
          action = ":update|:TestSuite<cr>";
        })
        (keymap {
          key = "ts";
          action = ":update|:TestSuite<cr>";
        })
      ];
    };
  };
}
