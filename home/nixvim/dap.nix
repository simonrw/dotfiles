{...}: let
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
  programs.nixvim = {
    plugins.dap = {
      enable = true;
      extensions.dap-go.enable = true;
      extensions.dap-python.enable = true;
      extensions.dap-ui.enable = true;
      extensions.dap-virtual-text.enable = true;
    };
    keymaps = [
      (keymap {
        key = "<f8>";
        action = ":lua require('dap').step_over()<cr>";
      })
      (keymap {
        key = "<F9>";
        action = ":lua require('dap').continue()<cr>";
      })
      (keymap {
        key = "<F10>";
        action = ":lua require('dap').step_into()<cr>";
      })
      (keymap {
        key = "<Shift><F9>";
        action = ":lua require('dap').step_out()<cr>";
      })
      (keymap {
        key = "<leader>b";
        action = ":lua require('dap').toggle_breakpoint()<cr>";
      })
    ];
  };
}
