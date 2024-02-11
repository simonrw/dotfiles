{...}: {
  programs.nixvim.plugins.treesitter-refactor = {
    enable = true;
    highlightDefinitions.enable = true;
    smartRename.enable = true;
  };
}
