{ pkgs }:
{
  enable = true;
  settings = {
    theme = "autumn_night";
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
