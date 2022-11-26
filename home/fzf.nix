{ pkgs, ... }:
{
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
    defaultCommand = ''rg --files --no-ignore --hidden --follow -g "!{.git,venv,node_modules}/*" 2> /dev/null'';
    defaultOptions = [
      "--color dark,matched_bg:-1"
      "--tiebreak begin"
      "--ansi"
      "--no-mouse"
      "--tabstop 4"
      "--inline-info"
    ];
    tmux.enableShellIntegration = true;
  };
}
