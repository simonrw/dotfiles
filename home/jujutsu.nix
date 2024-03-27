{...}: {
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "Simon Walker";
        email = "s.r.walker101@gmail.com";
      };
      ui = {
        default-command = "log";
        diff = {
          format = "git";
        };
      };
    };
  };
}
