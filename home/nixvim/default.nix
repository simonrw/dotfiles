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
in {
  imports = [
    ./completion.nix
    ./context.nix
    ./dap.nix
    ./diffview.nix
    ./gitsigns.nix
    ./lsp-format.nix
    ./lsp.nix
    ./markdown-preview.nix
    ./neotest.nix
    ./notifications.nix
    ./octo-nvim.nix
    ./oil.nix
    ./telescope.nix
    ./trouble.nix
    ./vim-test.nix
    ./zen-mode.nix
  ];
  config = {
    # custom overrides
    me.nixvim = {
      lsp = {
        enable = true;
        inlay-hints = false;
      };
      zen-mode.enable = true;
      completion = {
        enable = true;
        require-trigger = false;
        emoji = true;
      };
      context.enable = true;
      octo-nvim.enable = true;
      oil.enable = true;
      vim-test.enable = true;
      neotest.enable = false;
    };
    # defaults
    programs.nixvim = {
      enable = true;
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
        inccommand = "split";
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
        updatetime = 4000;
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
        # configure indent for hcl files
        {
          event = ["FileType"];
          pattern = ["hcl"];
          command = "setlocal shiftwidth=2 tabstop=2";
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
      };
      extraPlugins = with pkgs.vimPlugins; [
        vim-eunuch
        vim-unimpaired
        vim-rhubarb
        vim-repeat
        # rustaceanvim
        playground
        lsp-status-nvim
        nvim-nio
      ];
      extraConfigLua = ''
      '';
      extraFiles = {
        "colors/lucius.vim" = builtins.readFile ./colors/lucius.vim;
        "colors/srw256.vim" = builtins.readFile ./colors/srw256.vim;
      };
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
  };
}
