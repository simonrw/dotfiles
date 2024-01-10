# Default configuration file: https://github.com/atuinsh/atuin/blob/803b2fed5ce33cd08bf0c07b376c9a3f1e88f8ba/atuin-client/config.toml#L4
{
  programs.atuin = {
    enable = true;
    settings = {
      enter_accept = true;
      dialect = "uk";
      update_check = false;
      style = "compact";
      inline_height = 30;
      common_subcommands = [
        "cargo"
        "go"
        "git"
        "npm"
        "yarn"
        "pnpm"
        "kubectl"
        "docker"
      ];
    };
  };
}
