{
  config,
  pkgs,
  inputs,
  ...
}: {
  config = {
    home.packages = [
      inputs.neovim-nightly-overlay.packages.${pkgs.system}.default
    ];

    xdg.configFile."nvim" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/nix-config/home/neovim";
      recursive = true;
    };
  };
}
