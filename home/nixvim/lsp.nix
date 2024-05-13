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
    lua ? false,
  }: {
    inherit action mode key lua;
    options = {
      noremap = true;
      silent = true;
    };
  };

  cfg = config.me.nixvim.lsp;
in {
  options.me.nixvim.lsp = {
    enable = mkEnableOption "lsp";
  };
  config.programs.nixvim = mkIf cfg.enable {
    plugins = {
      lsp = {
        enable = true;
        servers = {
          gleam.enable = true;
          elmls.enable = true;
          nil_ls = {
            enable = true;
          };
          tsserver = {
            enable = true;
            extraOptions = {
              settings.typescript.format.indentSize = 2;
              settings.javascript.format.indentSize = 2;
            };
          };
          gopls.enable = true;
          rust-analyzer = {
            enable = true;
            # managed in projects
            installCargo = false;
            installRustc = false;
            settings = {
              cachePriming = {
                enable = true;
                numThreads = 0; # auto
              };
              check = {
                allTargets = true;
                command = "clippy";
              };
            };
          };
          pyright.enable = true;
        };
      };
    };
    keymaps = [
      (keymap {
        key = "gy";
        action = "vim.lsp.buf.type_definition";
        lua = true;
      })
      (keymap {
        key = "gi";
        action = "vim.lsp.buf.implementation";
        lua = true;
      })
      (keymap {
        key = "<leader>k";
        action = "vim.lsp.buf.hover";
        lua = true;
      })
      (keymap {
        key = "<leader>r";
        action = "vim.lsp.buf.rename";
        lua = true;
      })
      (keymap {
        key = "]d";
        action = "vim.diagnostic.goto_next";
        lua = true;
      })
      (keymap {
        key = "[d";
        action = "vim.diagnostic.goto_prev";
        lua = true;
      })
      (keymap {
        key = "<leader>a";
        action = "function() vim.lsp.buf.code_action({ source = { organizeImports = true } }) end";
        lua = true;
      })
      (keymap {
        mode = "i";
        key = "<C-h>";
        action = "vim.lsp.buf.signature_help";
        lua = true;
      })
    ];
  };
}
