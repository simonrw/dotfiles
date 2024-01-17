{
  programs.nixvim.plugins.dap = {
    enable = true;
    extensions.dap-go.enable = true;
    extensions.dap-python.enable = true;
    extensions.dap-ui.enable = true;
    extensions.dap-virtual-text.enable = true;
  };
}
