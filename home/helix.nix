{ pkgs }:
{
  enable = true;
  settings = {
    theme = "dark_plus";
    editor = {
      line-number = "relative";
      auto-pairs = false;
      true-color = true;
    };
  };
  languages = [
    {
      name = "python";
      language-server = {
        command = "${pkgs.pyright}/bin/pyright-langserver";
        args = [
          "--stdio"
        ];
      };
      config = { };
    }
  ];
}
