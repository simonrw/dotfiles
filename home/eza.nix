{ ... }:
{
  config = {
    programs.eza = {
      enable = true;
      git = true;
      icons = true;
      enableAliases = true;
      extraOptions = [
        "--group-directories-first"
        "--header"
        "--classify"
      ];
    };
  };
}
