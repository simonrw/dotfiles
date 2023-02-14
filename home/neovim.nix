{ config, pkgs, ... }:
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    defaultEditor = true;
    withPython3 = true;
    extraPython3Packages = (ps: with ps; [
      pynvim
      black
    ]);
    extraConfig = ''
      source ~/.config/nvim/nixinit.vim
    '';
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-hardtime
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
      papercolor-theme
      fidget-nvim
      nvim-base16

      lsp-format-nvim

      # treesitter
      nvim-treesitter.withAllGrammars
      nvim-treesitter-context
      playground
      plenary-nvim
    ];
  };
}
