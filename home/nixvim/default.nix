{pkgs, ...}: let
  keymap = {
    mode,
    key,
    action,
  }: {
    inherit mode key action;
    options = {
      noremap = true;
      silent = true;
    };
  };
in {
  config.programs.nixvim = {
    colorschemes.nord.enable = true;
    options = {
      autowrite = true;
      backspace = ["indent" "eol" "start"];
      breakindent = true;
      completeopt = ["menuone" "preview"];
      complete = ["." "w" "b" "u" "t" "i"];
      cursorline = false;
      expandtab = true;
      gdefault = true;
      hidden = true;
      history = 50;
      hlsearch = false;
      ignorecase = true;
      inccommand = "nosplit";
      incsearch = true;
      laststatus = 2;
      linebreak = true;
      list = false;
      mouse = "a";
      number = true;
      relativenumber = true;
      ruler = false;
      scrolloff = 8;
      shiftround = true;
      shiftwidth = 4;
      showcmd = true;
      smartcase = true;
      smartindent = true;
      synmaxcol = 1024;
      tabstop = 4;
      tags = [".git/tags"];
      termguicolors = true;
      textwidth = 0;
      timeoutlen = 500;
      ttimeoutlen = 10;
      wildmode = ["list:longest" "list:full"];
      wrap = false;
      winwidth = 80;
      updatetime = 50;
      switchbuf = ["useopen" "uselast"];
      conceallevel = 0;
      shortmess = "tToOFIWa";
      signcolumn = "yes";
      splitright = true;
      splitbelow = true;
      swapfile = true;
      writebackup = false;
      backup = false;
      backupcopy = "auto";
      backupdir = ["~/.vim/backup"];
      formatoptions = "jtcroql";
      undofile = true;
      modeline = true;
      lazyredraw = true;
      # TODO: handle with colours.nix
      background = "dark";
    };
    autoGroups = {
      lua-highlight.clear = true;
      terminal-settings.clear = true;
      diff-mode.clear = true;
      last-position.clear = true;
    };
    autoCmd = [
      {
        event = ["TextYankPost"];
        command = "lua require'vim.highlight'.on_yank()";
        group = "lua-highlight";
      }
      {
        event = ["TermOpen"];
        command = "startinsert";
        group = "terminal-settings";
      }
      {
        event = ["BufWritePost"];
        command = "if &diff == 1 | diffupdate | endif";
        group = "diff-mode";
      }
      {
        event = ["BufReadPost"];
        command = ''
          if &ft != 'gitcommit' && line("'\"") > 0 && line("'\"") <= line("$") |
              exe "normal g`\"" |
          endif
        '';
        group = "last-position";
      }
    ];
    userCommands = {
      T = {
        command = "split | resize 30 | term <args>";
        complete = "shellcmd";
        force = true;
        nargs = "*";
      };
    };
    keymaps = [
      # terminal map
      (keymap {
        mode = "t";
        key = "<Esc>";
        action = "<C-\\><C-n>";
      })
      (keymap {
        mode = "t";
        key = "<M-[>";
        action = "<Esc>";
      })
      (keymap {
        mode = "t";
        key = "<C-v><Esc>";
        action = "<Esc>";
      })
      # emulate vim-vinegar which stopped working
      (keymap {
        mode = "n";
        key = "-";
        action = ":Explore<Cr>";
      })
      (keymap {
        mode = "n";
        key = "<C-h>";
        action = "<C-w><C-h>";
      })
      (keymap {
        mode = "n";
        key = "<C-j>";
        action = "<C-w><C-j>";
      })
      (keymap {
        mode = "n";
        key = "<C-k>";
        action = "<C-w><C-k>";
      })
      (keymap {
        mode = "n";
        key = "<C-l>";
        action = "<C-w><C-l>";
      })
      # keep the cursor centered
      (keymap {
        mode = "n";
        key = "n";
        action = "nzzzv";
      })
      (keymap {
        mode = "n";
        key = "N";
        action = "Nzzzv";
      })
      (keymap {
        mode = "n";
        key = "/";
        action = "/\\v";
      })
      (keymap {
        mode = "n";
        key = "?";
        action = "?\\v";
      })
    ];
    globals = {
      mapleader = " ";
      nord_uniform_diff_background = 1;
    };
    plugins = {
      lualine.enable = true;
      gitsigns.enable = true;
      surround.enable = true;
      commentary.enable = true;
      fugitive.enable = true;
      telescope = {
        enable = true;
        defaults = {
          layout_strategy = "horizontal";
          layout_config.prompt_position = "top";
          sorting_strategy = "ascending";
        };
        extensions.fzf-native = {
          enable = true;
          fuzzy = true;
          overrideGenericSorter = true;
          overrideFileSorter = true;
          caseMode = "smart_case";
        };
        keymaps = {
          "<leader>f" = "git_files";
          "<leader>F" = "find_files";
          "gb" = "buffers";
          "<leader><space>" = "live_grep";
          "<leader>/" = "current_buffer_fuzzy_find";
          "gd" = "lsp_definitions";
          "gr" = "lsp_references";
          "<leader>g" = "diagnostics";
          "<leader>s" = "lsp_dynamic_workspace_symbols";
        };
      };
      lsp = {
        enable = true;
        servers = {
          tsserver.enable = true;
          rust-analyzer = {
            enable = true;
            installCargo = true;
            installRustc = true;
          };
          pyright.enable = true;
          ruff-lsp.enable = true;
          rnix-lsp.enable = true;
        };
      };
      treesitter = {
        enable = true;
      };
      treesitter-context = {
        enable = true;
        maxLines = 5;
      };
      treesitter-textobjects = {
        enable = true;
      };
      nvim-cmp = {
        enable = true;
        autoEnableSources = true;
        sources = [
          {name = "nvim_lsp";}
          {name = "path";}
          {name = "buffer";}
        ];
        mapping = {
          "<CR>" = "cmp.mapping.confirm({ select = true })";
          "<C-y>" = "cmp.mapping.confirm({ select = true })";
          "gy" = "vim.lsp.buf.type_definition()";
          "gi" = "vim.lsp.buf.implementation()";
          "<leader>k" = "vim.lsp.buf.hover()";
          "<leader>r" = "vim.lsp.buf.rename()";
          "]d" = "vim.diagnostic.goto_next()";
          "[d" = "vim.diagnostic.goto_prev()";
          "<leader>a" = "vim.lsp.buf.code_action({ source = { organizeImports = true } })";
          "<C-h>" = {
            action = "vim.lsp.buf.signature_help()";
            modes = ["i"];
          };
          "<C-Space>" = "cmp.mapping.complete()";
        };
      };
    };
    extraPlugins = with pkgs.vimPlugins; [
      vim-eunuch
      vim-unimpaired
      vim-rhubarb
      vim-repeat
      vim-test
      rustaceanvim
      playground
      lsp-status-nvim
    ];
  };
}
