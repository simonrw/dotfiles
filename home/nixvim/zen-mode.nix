{pkgs, ...}: {
  programs.nixvim.extraPlugins = with pkgs.vimPlugins; [
    zen-mode-nvim
    twilight-nvim
  ];
}
