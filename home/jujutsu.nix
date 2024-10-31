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
        diff = {
          format = "git";
        };
      };
      # https://willhbr.net/2024/08/18/understanding-revsets-for-a-better-jj-log-output/
      revsets.log = "@ | ancestors(trunk()..(visible_heads() & mine()), 2) | trunk()";
    };
  };
}
