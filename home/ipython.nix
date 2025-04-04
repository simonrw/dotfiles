{config, ...}: let
  colours =
    if config.me.theme == "github-light"
    then "LightBG"
    else "Linux";
in {
  home.file.".ipython/profile_default/ipython_config.py" = {
    text = ''
      c = get_config()
      c.TerminalIPythonApp.display_banner = False
      c.TerminalIPythonApp.log_format = "[%(asctime)s|%(name)s] %(message)s"
      c.TerminalInteractiveShell.colors = "${colours}"
      c.TerminalInteractiveShell.autoindent = True
      c.TerminalInteractiveShell.autocall = 0
    '';
  };
}
