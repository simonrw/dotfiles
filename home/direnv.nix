{...}: {
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    silent = true;
    config = {
      global = {
        load_dotenv = true;
        strict_env = true;
        warn_timeout = "0";
        hide_env_diff = true;
      };
    };
  };
}
