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
    plugins.trouble = {
      enable = true;
      settings = {
        icons = false;
        use_diagnostic_signs = false;
        auto_preview = false;
      };
    };
    keymaps = [
      (keymap {
        key = "yot";
        action = "function() require('trouble').toggle() end";
        lua = true;
      })
    ];
  };
}
