{...}: {
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "Simon Walker";
        email = "s.r.walker101@googlemail.com";
      };
      ui = {
        default-command = "status";
        diff = {
          format = "git";
        };
      };
    };
  };
}
