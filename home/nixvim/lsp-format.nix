{
  plugins.lsp-format = {
    enable = true;
    settings = {
      nix = {
        exclude = [
          "nixd"
          "pyright"
        ];
      };
    };
  };
}
