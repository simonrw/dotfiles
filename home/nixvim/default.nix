{pkgs, ...}: let
  keymap = {
    key,
    action,
    mode ? "n",
  }: {
    inherit mode key action;
    options = {
      noremap = true;
      silent = true;
    };
  };
in {
  config.programs.nixvim = {
    enable = true;
    colorschemes.nord.enable = true;
    editorconfig.enable = false;
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
      number = false;
      relativenumber = false;
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
      grepprg = "rg --vimgrep";
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
        key = "-";
        action = ":Explore<Cr>";
      })
      (keymap {
        key = "<C-h>";
        action = "<C-w><C-h>";
      })
      (keymap {
        key = "<C-j>";
        action = "<C-w><C-j>";
      })
      (keymap {
        key = "<C-k>";
        action = "<C-w><C-k>";
      })
      (keymap {
        key = "<C-l>";
        action = "<C-w><C-l>";
      })
      # keep the cursor centered
      (keymap {
        key = "n";
        action = "nzzzv";
      })
      (keymap {
        key = "N";
        action = "Nzzzv";
      })
      (keymap {
        key = "/";
        action = "/\\v";
      })
      (keymap {
        key = "?";
        action = "?\\v";
      })
      (keymap {
        key = "gy";
        action = "vim.lsp.buf.type_definition()";
      })
      (keymap {
        key = "gi";
        action = "vim.lsp.buf.implementation()";
      })
      (keymap {
        key = "<leader>k";
        action = "vim.lsp.buf.hover()";
      })
      (keymap {
        key = "<leader>r";
        action = "vim.lsp.buf.rename()";
      })
      (keymap {
        key = "]d";
        action = "vim.diagnostic.goto_next()";
      })
      (keymap {
        key = "[d";
        action = "vim.diagnostic.goto_prev()";
      })
      (keymap {
        key = "<leader>a";
        action = "vim.lsp.buf.code_action({ source = { organizeImports = true } })";
      })
      (keymap {
        mode = "i";
        key = "<C-h>";
        action = "vim.lsp.buf.signature_help()";
      })
      (keymap {
        key = "<leader>A";
        action = "<cmd>AerialToggle!<cr>";
      })
      # fugitive
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
      # vim-test
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
    globals = {
      mapleader = " ";
      nord_uniform_diff_background = 1;
      # vim test
      "test#python#runner" = "pytest";
      "test#javascript#reactscripts#options" = "--watchAll=false";
      "test#strategy" = "basic";
    };
    luaLoader.enable = true;
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
        extraOptions = {
          git_files = {
            disable_devicons = true;
          };
          find_files = {
            disable_devicons = true;
          };
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
          gopls = {
            enable = true;
            installLanguageServer = false;
          };
          rust-analyzer = {
            enable = true;
            # managed in projects
            installCargo = false;
            installRustc = false;
          };
          pyright.enable = true;
          ruff-lsp.enable = true;
          rnix-lsp.enable = true;
        };
      };
      treesitter = {
        enable = true;
        disabledLanguages = [
          "gitcommit"
        ];
      };
      treesitter-context = {
        enable = true;
        maxLines = 5;
      };
      treesitter-textobjects = {
        enable = true;
      };
      nix.enable = true;
      luasnip = {
        enable = true;
        extraConfig = {
          history = true;
          updateevents = "TextChanged,TextChangedI";
          enable_autosnippets = true;
        };
      };
      cmp_luasnip.enable = true;
      nvim-cmp = {
        enable = true;
        autoEnableSources = true;
        preselect = "None";
        sources = [
          {name = "nvim_lsp";}
          {name = "path";}
          {name = "buffer";}
          {name = "luasnip";}
        ];
        mapping = {
          "<C-p>" = "cmp.mapping.select_prev_item({ select = true })";
          "<C-n>" = "cmp.mapping.select_next_item({ select = true })";
          "<C-y>" = "cmp.mapping.confirm({ select = true })";
          "<C-Space>" = "cmp.mapping.complete()";
          "<CR>" = "cmp.mapping.confirm({ select = true })";
          "<C-e>" = "cmp.config.disable";
        };
        snippet.expand = "luasnip";
      };
    };
    extraPlugins = with pkgs.vimPlugins; [
      aerial-nvim
      vim-eunuch
      vim-unimpaired
      vim-rhubarb
      vim-repeat
      vim-test
      # rustaceanvim
      playground
      lsp-status-nvim
      vim-test
    ];
    extraConfigLua = ''
      -- set up aerial-nvim
      require('aerial').setup()
    '';
  };
}
