{...}: {
  config = {
    programs.eza = {
      enable = true;
      enableAliases = true;
      extraOptions = [
        "--group-directories-first"
        "--header"
      ];
    };
  };
}
