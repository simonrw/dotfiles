{
  config,
  pkgs,
  isLinux,
  ...
}: let
  inherit (pkgs) lib;

  # style output of go "glamour" programs e.g. gh
  glamour-style =
    if config.me.dark-mode
    then "dark"
    else "light";
in {
  programs.fish = {
    enable = true;
    interactiveShellInit =
      ''
        set -x GLAMOUR_STYLE ${glamour-style}
        set -x PAGER "${pkgs.bat}/bin/bat"
        set -x PYTHONUNBUFFERED 1
      ''
      + (builtins.readFile ./fish/config.fish);
    shellAliases =
      {
        add-keys = ''ssh-add (find ~/.ssh - maxdepth 1 - type f - name "id_rsa*" | grep - v pub | grep - v bak)'';
        cat = "${pkgs.bat}/bin/bat";
        clear-pycs = "find { $PWD } -name '*.pyc' -delete";
        da = "${pkgs.direnv}/bin/direnv allow";
        de = "${pkgs.direnv}/bin/direnv edit";
        es = ''exec $SHELL'';
        gcpr = "${pkgs.gh}/bin/gh pr create -a @me --label 'semver: patch'";
        gcprd = "${pkgs.gh}/bin/gh pr create -a @me --draft --label 'semver: patch'";
        gpr = "${pkgs.git}/bin/git pull --rebase";
        grep = "${pkgs.ripgrep}/bin/rg";
        less = "${pkgs.bat}/bin/bat";
        lr = "thor";
        more = "${pkgs.bat}/bin/bat";
        nl = "nix-locate --regex --top-level";
        nr = "nix repl --file '<nixpkgs>'";
        ns = "nix shell";
        nix-shell = "nix-shell --command fish";
        ntfy = "notify-wrapper";
        pylab = "ipython - -pylab";
        sourceenv = "source ./venv/bin/activate";
        ta = "_tmux_attach";
        thor = "${pkgs.eza}/bin/eza -s modified -l";
        tl = "tmux-last";
        trash = "${pkgs.python3Packages.send2trash}/bin/send2trash";
        tree = "${pkgs.eza}/bin/eza -T";
        vup = ''nvim --headless "+Lazy! sync" +qa'';
        vimdiff = "nvim -d";
        pydoc = "python -m pydoc";
        project = "listprojects";
      }
      // lib.optionalAttrs isLinux {
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
      };
    shellAbbrs = {
      c = "cargo";
      d = "dev";
      g = "git";
      gco = "git checkout";
      gp = "git pull";
      ipy = "ipython";
      k = "kubectl";
      nd = "nix develop --command fish";
      n = "notify-wrapper";
      py = "python";
      t = "testsearch";
      v = "vim";
    };
    plugins = [
      {
        name = "fish-foreign-env";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-foreign-env";
          rev = "b3dd471bcc885b597c3922e4de836e06415e52dd";
          sha256 = "3h03WQrBZmTXZLkQh1oVyhv6zlyYsSDS7HTHr+7WjY8=";
        };
      }
      {
        name = "fish-ssh-agent";
        src = pkgs.fetchFromGitHub {
          owner = "ivakyb";
          repo = "fish_ssh_agent";
          rev = "c7aa080d5210f5f525d078df6fdeedfba8db7f9b";
          sha256 = "v9VZY5DCo+iWZawRKVgFvsi33UKwtriSpUzrMhL0S14=";
        };
      }
      {
        name = "nix-env";
        src = pkgs.fetchFromGitHub {
          owner = "lilyball";
          repo = "nix-env.fish";
          rev = "7b65bd228429e852c8fdfa07601159130a818cfa";
          sha256 = "RG/0rfhgq6aEKNZ0XwIqOaZ6K5S4+/Y5EEMnIdtfPhk=";
        };
      }
    ];
    functions = {
      _tmux_attach = {
        description = "Attach to a tmux session";
        body = ''
          if test (count $argv) -gt 0
            tmux attach -t $argv[1]
          else
            tmux attach
          end
        '';
      };
      ssh-notmux = {
        description = "SSH into a host disabling automatic tmux on the destination host";
        body = ''
          if test (count $argv) -gt 1
              ssh -t $argv[1] env TMUX_DISABLED=1 $argv[2..-1]
          else
              ssh -t $argv[1] env TMUX_DISABLED=1 fish
          end
        '';
        wraps = "ssh";
      };
      fish_greeting = {
        description = "Greeting to show when starting a fish shell";
        body = "";
      };
      mcd = {
        description = "Create and move into new directory";
        body = "mkdir -p $argv[1]; and cd $argv[1]";
      };
      fish_prompt = {
        description = "Write out the prompt";
        body = builtins.readFile ./fish/prompt.fish;
      };
      # has to be a function as it affects the current shell
      kctx = {
        description = "Choose a kubeconfig context";
        body = builtins.readFile ./fish/kctx.fish;
      };
    };
  };
}
