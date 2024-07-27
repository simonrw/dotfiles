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
        editor = "code -w";
        diff = {
          format = "git";
        };
      };
    };
  };
}
