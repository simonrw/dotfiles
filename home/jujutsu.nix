{...}: {
  programs.jujutsu = {
    enable = true;
    settings = {
      user = {
        name = "Simon Walker";
        email = "s.r.walker101@googlemail.com";
      };
      ui = {
        pager = "delta";
        default-command = "status";
        diff-editor = "diffedit3";
        diff = {
          format = "git";
        };
      };
    };
  };
}
