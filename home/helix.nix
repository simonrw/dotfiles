{ pkgs }:
{
  enable = true;
  settings = {
    theme = "dark_plus";
    editor = {
      line-number = "relative";
      auto-pairs = false;
      true-color = true;
      indent-guides.render = true;
      shell = [ "${pkgs.fish}/bin/fish" "-c" ];
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
