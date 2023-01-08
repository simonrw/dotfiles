{ config, pkgs, isDarwin, ... }:
{
  programs.tmux = {
    enable = true;
    aggressiveResize = true;
    baseIndex = 1;
    clock24 = true;
    escapeTime = 0;
    historyLimit = 10000;
    keyMode = "vi";
    sensibleOnTop = true;
    shortcut = "s";
    terminal = "screen-256color";
    shell = "${pkgs.fish}/bin/fish";
    secureSocket = true;
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = ''
          set -g @resurrect-save 'W'
          set -g @resurrect-strategy-nvim 'session'
          set -g @resurrect-capture-pane-contents 'on'
        '';
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
        '';
      }
      tmuxPlugins.open
    ];
    extraConfig = with pkgs;
      let
        colourschemeFile =
          if config.dark-mode then
            ./tmux/dark-colourscheme.conf
          else ./tmux/light-colourscheme.conf;

        commonFiles = with builtins; [
          (readFile ./tmux/tmux.conf)
          (readFile colourschemeFile)
        ];

        darwinFiles = lib.optionals isDarwin [
          (builtins.readFile ./tmux/tmux-osx.conf)
        ];
        # these require access to the pkgs attribute set, so place them here
        customLines = [
          ''
            bind-key a run-shell -b ${pkgs.listprojects}/bin/project
          ''
        ];
      in
      (builtins.concatStringsSep "\n" (commonFiles ++ darwinFiles ++ customLines));
  };
}
