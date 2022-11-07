{ pkgs }:
{
  enable = true;
  viAlias = true;
  vimAlias = true;
  vimdiffAlias = true;
  withPython3 = true;
  extraPython3Packages = (ps: with ps; [
    pynvim
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
    octo-nvim
    lualine-nvim

    # lsps
    nvim-lspconfig
    lsp_extensions-nvim
    lsp-status-nvim
    lsp-format-nvim

    # completion
    cmp-nvim-lsp
    cmp-buffer
    nvim-cmp
    cmp-vsnip
    cmp-emoji
    vim-vsnip
    lspkind-nvim

    # treesitter
    # nvim-treesitter-context
    # playground
    plenary-nvim
  ];
}
