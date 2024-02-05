{...}: {
  programs.nixvim.plugins.treesitter-refactor = {
    enable = true;
    highlightDefinitions.enable = true;
    navigation = {
      enable = true;
      keymaps = {
        gotoDefinition = null;
        gotoDefinitionLspFallback = "gd";
      };
    };
    smartRename.enable = true;
  };
}
