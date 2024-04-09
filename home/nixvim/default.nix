{pkgs, ...}: let
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

  # custom plugins
  nvim-nio = pkgs.vimUtils.buildVimPlugin {
    pname = "nvim-nio";
    version = "1.9.0";
    src = pkgs.fetchFromGitHub {
      owner = "nvim-neotest";
      repo = "nvim-nio";
      rev = "v1.9.0";
      hash = "sha256-ZRYclqsgAvlRBwb59XHlqVat7CxUJTH1rD6QLwh1ang=";
    };
  };
in {
  imports = [
    ./dap.nix
    ./gitsigns.nix
    ./lsp-format.nix
    ./lsp.nix
    ./markdown-preview.nix
    ./telescope.nix
    ./trouble.nix
    ./zen-mode.nix
    ./diffview.nix
  ];
  config.programs.nixvim = {
    enable = true;
    editorconfig.enable = false;
    colorschemes.nord = {
      enable = true;
      settings.uniform_diff_background = true;
    };
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
      # support closing fugitive window with 'q'
      {
        event = ["FileType"];
        pattern = ["fugitive"];
        command = "nmap <buffer> q gq";
      }
      # auto wrap markdown files
      {
        event = ["FileType"];
        pattern = ["markdown"];
        command = "setlocal wrap";
      }
      {
        event = ["FileType"];
        pattern = ["javascript" "typescript"];
        command = "setlocal tabstop=2 shiftwidth=2";
      }
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
      # copy the entire buffer to the system clipboard
      (keymap {
        key = "cp";
        action = ":0,$y+<cr>";
      })
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
      (keymap {
        key = "<leader>W";
        action = '':mksession!|echo "Session saved"<cr>'';
      })
      (keymap {
        key = "Q";
        action = ":call ToggleList('Quickfix List', 'c')<cr>";
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
        key = "<leader>A";
        action = "<cmd>Lspsaga outline<cr>";
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
      # vim test
      "test#python#runner" = "pytest";
      "test#javascript#reactscripts#options" = "--watchAll=false";
      "test#strategy" = "basic";
    };
    luaLoader.enable = true;
    plugins = {
      surround.enable = true;
      commentary.enable = true;
      fugitive.enable = true;
      treesitter = {
        enable = true;
        disabledLanguages = [
          "gitcommit"
        ];
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
      cmp = {
        enable = true;
        settings = {
          autoEnableSources = true;
          preselect = "None";
          sources = [
            {name = "copilot";}
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
            "<C-e>" = "cmp.config.disable";
          };
          snippet.expand = ''
            function(args)
              require('luasnip').lsp_expand(args.body)
            end
          '';
        };
      };
    };
    extraPlugins = with pkgs.vimPlugins; [
      vim-eunuch
      vim-unimpaired
      vim-rhubarb
      vim-repeat
      vim-test
      # rustaceanvim
      playground
      lsp-status-nvim
      vim-test
      nvim-nio
    ];
    extraConfigLua = ''
    '';
    extraConfigVim = ''
      " Add mapping to open/close the quickfix list
      " Taken from: http://vim.wikia.com/wiki/Toggle_to_open_or_close_the_quickfix_window
      function! GetBufferList()
        redir =>buflist
        silent! ls!
        redir END
        return buflist
      endfunction

      function! ToggleList(bufname, pfx)
        let buflist = GetBufferList()
        for bufnum in map(filter(split(buflist, '\n'), 'v:val =~ "'.a:bufname.'"'), 'str2nr(matchstr(v:val, "\\d\\+"))')
          if bufwinnr(bufnum) != -1
            exec(a:pfx.'close')
            return
          endif
        endfor
        if a:pfx == 'l' && len(getloclist(0)) == 0
            echohl ErrorMsg
            echo "Location List is Empty."
            return
        endif
        let winnr = winnr()
        exec(a:pfx.'open')
        if winnr() != winnr
          wincmd p
        endif
      endfunction
    '';
  };
}
