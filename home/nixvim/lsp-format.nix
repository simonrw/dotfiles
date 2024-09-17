{
  plugins.lsp-format = {
    enable = true;
    setup = {
      nix = {
        exclude = [
          "nixd"
          "pyright"
        ];
      };
    };
  };
}
