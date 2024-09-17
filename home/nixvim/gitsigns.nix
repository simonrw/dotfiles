{lib, ...}: let
  rebind = mode: mapDefn:
    lib.attrsets.mapAttrsToList (key: value: {
      mode = mode;
      key = key;
      action = value;
      lua = true;
      options = {
        silent = true;
      };
    })
    mapDefn;
  nnoremap = rebind "n";
  vnoremap = rebind "v";
in {
  plugins.gitsigns.enable = true;
  keymaps =
    [
      {
        mode = "n";
        key = "]c";
        action = ''
          function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() require('gitsigns').next_hunk() end)
            return '<Ignore>'
          end
        '';
        lua = true;
        options = {
          silent = true;
          expr = true;
        };
      }
      {
        mode = "n";
        key = "[c";
        action = ''
          function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() require('gitsigns').prev_hunk() end)
            return '<Ignore>'
          end
        '';
        lua = true;
        options = {
          silent = true;
          expr = true;
        };
      }
    ]
    ++ (nnoremap {
      "<leader>hs" = "require('gitsigns').stage_hunk";
      "<leader>hr" = "require('gitsigns').reset_hunk";
      "<leader>hS" = "require('gitsigns').stage_buffer";
      "<leader>hu" = "require('gitsigns').undo_stage_hunk";
      "<leader>hR" = "require('gitsigns').reset_buffer";
      "<leader>hp" = "require('gitsigns').preview_hunk";
      "<leader>hb" = "function() require('gitsigns').blame_line { full = true } end";
      "<leader>hd" = "require('gitsigns').diffthis";
      "<leader>hD" = "function() require('gitsigns').diffthis('~') end";
    })
    ++ (vnoremap {
      "<leader>hs" = "function() require('gitsigns').stage_hunk { vim.fn.line('.'), vim.fn.line('v') } end";
      "<leader>hr" = "function() require('gitsigns').reset_hunk { vim.fn.line('.'), vim.fn.line('v') } end";
    });
}
