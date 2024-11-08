{
  imports = [
    ./base.nix
    ./cody.nix
    ./completion.nix
    ./context.nix
    ./dap.nix
    ./diffview.nix
    ./diffview.nix
    ./fugitive.nix
    ./gitsigns.nix
    ./hardtime.nix
    ./lsp-format.nix
    ./lsp.nix
    ./markdown-preview.nix
    ./mini.nix
    ./neogit.nix
    ./neotest.nix
    ./notifications.nix
    ./octo-nvim.nix
    ./oil.nix
    ./render-markdown.nix
    ./statusbar.nix
    ./telescope.nix
    ./trouble.nix
    ./vim-test.nix
    ./which-key.nix
    ./zen-mode.nix
  ];
  config = {
    # custom overrides
    me.nixvim = {
      trouble.enable = false;
      dap.enable = false;
      lsp = {
        enable = true;
        inlay-hints = true;
      };
      zen-mode.enable = false;
      completion = {
        enable = true;
        require-trigger = true;
        emoji = true;
        supermaven = false;
      };
      context = {
        enable = true;
        method = "treesitter-context";
      };
      octo-nvim.enable = false;
      oil.enable = true;
      vim-test.enable = true;
      neotest.enable = false;
      notifications.enable = false;
      statusbar.enable = false;
      mini.enable = false;
      which-key.enable = false;
      diffview.enable = false;
      render-markdown.enable = false;
      fugitive.enable = true;
    };
  };
}
