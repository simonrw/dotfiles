{ config, pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    withPython3 = true;
    extraPython3Packages = (ps: with ps; [
      pynvim
      black
      debugpy
    ]);
    extraConfig = ''
      source ~/.config/nvim/nixinit.vim
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
      vim-gutentags
      vim-toml
      onedark-nvim
      vim-svelte
      vim-terraform
      rust-vim
      papercolor-theme
      nvim-base16
      aerial-nvim
      # has native plugin so requires nix
      telescope-fzf-native-nvim

      # debugging
      nvim-dap
      nvim-dap-python
      nvim-dap-go
      nvim-dap-ui
      nvim-dap-virtual-text

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
          p.tree-sitter-gitignore
          p.tree-sitter-go
          p.tree-sitter-graphql
          p.tree-sitter-http
          p.tree-sitter-javascript
          p.tree-sitter-json
          p.tree-sitter-lua
          p.tree-sitter-make
          p.tree-sitter-markdown
          p.tree-sitter-nix
          p.tree-sitter-norg
          p.tree-sitter-python
          p.tree-sitter-query
          p.tree-sitter-rust
          p.tree-sitter-sql
          p.tree-sitter-toml
          p.tree-sitter-typescript
          p.tree-sitter-yaml
          p.tree-sitter-zig
        ]))
      nvim-treesitter-textobjects
      nvim-treesitter-context
      playground
      plenary-nvim
      lsp-status-nvim
      dracula-nvim
      nord-nvim
    ];
  };
}
