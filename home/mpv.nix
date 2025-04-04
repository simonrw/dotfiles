{isLinux, ...}: {
  config = {
    programs.mpv = {
      enable = isLinux;
    };
  };
}
