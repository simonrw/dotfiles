{ config, pkgs, ... }:
let
  dark-mode-config =
    if config.dark-mode then
      '' 
      set background=dark
      colorscheme srw256
      '' else ''
      set background=light
      colorscheme PaperColor
      hi TreesitterContext guibg=#dddddd
    '';
in
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true;
    extraPython3Packages = (ps: with ps; [
      pynvim
      black
    ]);
    extraConfig = ''
      source ~/.config/nvim/nixinit.vim
    '' + dark-mode-config + ''
      hi Normal guibg=none
      let g:linenr_background = 'none'
      execute 'highlight TelescopeNormal guibg=' . g:linenr_background
      execute 'highlight LineNr guibg=' . g:linenr_background
      execute 'highlight SignColumn guibg=' . g:linenr_background
      execute 'highlight GitGutterAdd guibg=' . g:linenr_background
      execute 'highlight GitGutterDelete guibg=' . g:linenr_background
      execute 'highlight GitGutterChange guibg=' . g:linenr_background
      highlight TabLine guibg=none
      highlight TabLineSel guibg=none
      highlight TabLineFill guibg=none
      execute 'highlight DiagnosticSignError ctermfg=1 guifg=Red guibg=' . g:linenr_background
      execute 'highlight DiagnosticSignHint ctermfg=7 guifg=LightGrey guibg=' . g:linenr_background
      execute 'highlight DiagnosticSignInfo ctermfg=4 guifg=LightBlue guibg=' . g:linenr_background
      execute 'highlight DiagnosticSignWarn ctermfg=3 guifg=Orange guibg=' . g:linenr_background
      highlight DiagnosticUnderlineHint guifg=Grey guisp=Grey
    '';
    plugins = with pkgs.vimPlugins; [
      vim-nix
      # pkgs.vimPlugins.skim
      # skim-vim
      vim-surround
      vim-unimpaired
      vim-commentary
      vim-eunuch
      vim-fugitive
      vim-rhubarb
      vim-repeat
      # vim-tmux-runner
      # vim-conflicted
      vim-test
      vim-easy-align
      fugitive-gitlab-vim
      markdown-preview-nvim
      vim-gitgutter
      telescope-nvim
      telescope-ui-select-nvim
      telescope-fzf-native-nvim
      vim-gutentags
      editorconfig-nvim
      vim-toml
      vim-svelte
      vim-terraform
      rust-vim
      lualine-nvim
      which-key-nvim
      papercolor-theme
      fidget-nvim

      lsp-format-nvim

      # treesitter
      (nvim-treesitter.withPlugins
        (p: [
          p.tree-sitter-bash
          p.tree-sitter-c
          p.tree-sitter-cmake
          p.tree-sitter-cpp
          p.tree-sitter-dot
          p.tree-sitter-elm
          p.tree-sitter-fish
          p.tree-sitter-git_rebase
          p.tree-sitter-gitignore
          p.tree-sitter-go
          p.tree-sitter-graphql
          p.tree-sitter-http
          p.tree-sitter-javascript
          p.tree-sitter-json
          p.tree-sitter-lua
          p.tree-sitter-make
          p.tree-sitter-markdown
          # See https://github.com/nvim-treesitter/nvim-treesitter/issues/4143
          # p.tree-sitter-nix
          p.tree-sitter-python
          p.tree-sitter-query
          p.tree-sitter-rust
          p.tree-sitter-sql
          p.tree-sitter-toml
          p.tree-sitter-yaml
          p.tree-sitter-zig
        ]))
      nvim-treesitter-context
      playground
      plenary-nvim
    ];
  };
}
