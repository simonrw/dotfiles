{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.me.editor;

  binary-name =
    {
      helix = "hx";
      neovim = "nvim";
    }
    .${cfg.name};
in {
  options.me.editor = {
    name = mkOption {
      type = types.enum [
        "helix"
        "neovim"
      ];
    };
  };

  config = {
    programs.fish = {
      shellAliases = {
        vim = binary-name;
        nvim = binary-name;
      };
      interactiveShellInit = ''
        set -x EDITOR ${binary-name}
      '';
    };
    programs.git.extraConfig.core.editor = binary-name;
  };
}
