{ ... }:
{
  programs.contour = {
    enable = true;

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
