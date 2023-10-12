{ hostname, ... }:
{
  programs.contour = {
    enable = hostname != "macvm";

    settings = {
      profiles.main = {
        terminal_size = {
          columns = 120;
          lines = 30;
        };
        scrollbar.position = "hidden";
        cursor.shape = "block";
        colors = "default";
      };
    };
  };
}
