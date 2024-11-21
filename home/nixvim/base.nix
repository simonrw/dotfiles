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

  # TOOD: unify with home configuration
  colour-theme = "catppuccin-macchiato";

  theme-lua =
    {
      poimandres = ''
        require('poimandres').setup({})
        vim.cmd("colorscheme poimandres")
      '';
      papercolor = ''
        vim.cmd.highlight({ "TreesitterContext", "guibg=#f6f6ff" })
        vim.cmd [[set background=light]]
        vim.cmd [[colorscheme PaperColorSlim]]
      '';
      nord = ''
        vim.g.nord_disable_background = true
        vim.g.nord_italic = false
        vim.cmd [[colorscheme nord]]
        vim.cmd.highlight({ "@comment", "guifg=#d08770" })
        vim.cmd.highlight({ "TreesitterContext", "guibg=#363c4a" })
        -- yellow
        vim.cmd.highlight({ "@markup.raw", "guifg=#ebcb8b" })
      '';
      github-light = ''
        vim.cmd [[set background=light]]
        vim.cmd [[colorscheme github_light]]
        vim.cmd.highlight({ "DiagnosticError", "guifg=Red" })
        vim.cmd.highlight({ "DiagnosticHint", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticWarn", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticInfo", "guifg=LightBlue" })
        vim.cmd.highlight({ "DiagnosticFloatingError", "guifg=Red" })
        vim.cmd.highlight({ "DiagnosticFloatingHint", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticFloatingInfo", "guifg=LightBlue" })
        vim.cmd.highlight({ "DiagnosticFloatingWarn", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticVirtualTextError", "guifg=Red" })
        vim.cmd.highlight({ "DiagnosticVirtualTextHint", "guifg=Orange" })
        vim.cmd.highlight({ "DiagnosticVirtualTextInfo", "guifg=LightBlue" })
        vim.cmd.highlight({ "DiagnosticVirtualTextWarn", "guifg=Orange" })
        vim.cmd.highlight({ "Comment", "guifg=#e69340" })
        vim.cmd.highlight({ "TreesitterContext", "guibg=#eeeeee" })
      '';
      # TODO: migrate these to nixvim
      one-dark = ''
        colorscheme onedark
      '';
      catppuccin-frappe = ''
        vim.cmd([[
          set background=dark
          colorscheme catppuccin-frappe
        ]])
      '';
      dracula = ''
        set background=dark
        colorscheme dracula
      '';
      github = ''
        vim.opt.background = "dark"
        -- overrides
        vim.cmd.highlight({ "Comment", "guifg=#e69340" })
        vim.cmd.highlight({ "TSComment", "guifg=#e69340" })
        vim.cmd.highlight({ "Normal", "guibg=none" })
        vim.cmd.highlight({ "NormalNC", "guibg=none" })
        -- set background colour
        vim.cmd "colorscheme github_dark"
      '';
      gruvbox = ''
        set background=dark
        let g:gruvbox_contrast_dark = "hard"
        colorscheme gruvbox
        highlight Comment guifg=#e69340   " brighten comments
        highlight TSComment guifg=#e69340   " brighten comments
      '';
      monochrome = ''
        set background=dark
        colorscheme fogbell
        hi Normal guibg=none
      '';
      srw = ''
        vim.cmd([[
          set background=dark
          colorscheme srw256
          hi Normal guibg=none
          let g:linenr_background = 'none'
          execute 'highlight TelescopeNormal guibg=' . g:linenr_background
          execute 'highlight LineNr guibg=' . g:linenr_background
          execute 'highlight SignColumn guibg=' . g:linenr_background
          highlight TabLine guibg=none
          highlight TabLineSel guibg=none
          highlight TabLineFill guibg=none
          execute 'highlight DiagnosticSignError ctermfg=1 guifg=Red guibg=' . g:linenr_background
          execute 'highlight DiagnosticSignHint ctermfg=7 guifg=LightGrey guibg=' . g:linenr_background
          execute 'highlight DiagnosticSignInfo ctermfg=4 guifg=LightBlue guibg=' . g:linenr_background
          execute 'highlight DiagnosticSignWarn ctermfg=3 guifg=Orange guibg=' . g:linenr_background
          highlight DiagnosticUnderlineHint guifg=Grey guisp=Grey
        ]])
      '';
      catppuccin-latte = ''
        vim.cmd([[
          set background=light
          colorscheme catppuccin-latte
        ]])
      '';
      catppuccin-macchiato = ''
        -- remove underline from bottom of highlight section
        vim.cmd([[
          set background=dark
          colorscheme catppuccin-macchiato
        ]])
        vim.cmd.highlight({ 'TreesitterContextBottom', 'gui=none' })
      '';
      catppuccin-mocha = ''
        -- remove underline from bottom of highlight section
        vim.cmd([[
          set background=dark
          colorscheme catppuccin-mocha
        ]])
        vim.cmd.highlight({ 'TreesitterContextBottom', 'gui=none' })
      '';
    }
    .${colour-theme};
in {
  editorconfig.enable = false;
  opts = {
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
    AwsTestNearest = {
      command = "TestNearest TEST_TARGET=AWS_CLOUD AWS_PROFILE=ls-sandbox SNAPSHOT_UPDATE=1 <args>";
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
      action = ":e %:h<Cr>";
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
    web-devicons.enable = false;
    vim-surround.enable = true;
    commentary.enable = true;
    treesitter = {
      enable = true;
      gccPackage = null;
      nodejsPackage = null;
      settings.highlight = {
        enable = true;
      };
      settings.indent.enable = false;
    };
    treesitter-textobjects = {
      enable = true;
      select = {
        enable = true;
        keymaps = {
          "af" = "@function.outer";
          "if" = "@function.inner";
          "ac" = "@class.outer";
          "ic" = "@class.inner";
        };
        selectionModes = {
          "@function.outer" = "V";
          "@function.inner" = "V";
          "@class.outer" = "V";
          "@class.inner" = "V";
        };
      };
      move = {
        enable = true;
        setJumps = true;
        gotoNextStart = {
          "]m" = "@function.outer";
          "]M" = "@class.outer";
        };
        gotoPreviousStart = {
          "[m" = "@function.outer";
          "[M" = "@class.outer";
        };
      };
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
    catppuccin-nvim
  ];
  extraConfigLua = theme-lua;
  extraFiles = {
    "colors/lucius.vim".source = ./colors/lucius.vim;
    "colors/srw256.vim".source = ./colors/srw256.vim;
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
}
