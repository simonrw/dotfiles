{
  config,
  pkgs,
  isLinux,
  ...
}: let
  inherit (pkgs) lib;

  colour-theme-name =
    if config.me.dark-mode
    then "Tomorrow Night Bright"
    else (throw "Light theme not configured yet");

  # style output of go "glamour" programs e.g. gh
  glamour-style =
    if config.me.dark-mode
    then "dark"
    else "light";
in {
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
        set -x SHELL (command -v fish)

        set -x BUILD_PREFIX {$HOME}/.local
        set -x PATH {$BUILD_PREFIX}/bin {$HOME}/.bin {$HOME}/.poetry/bin /usr/local/bin {$HOME}/.cargo/bin {$HOME}/bin {$GOPATH}/bin {$HOME}/.npm-packages/bin {$PATH}
        set -x NODE_PATH {$HOME}/.npm-packages/lib/node_modules
        set -x GOPATH {$HOME}/dev/gocode
        set -x REVIEW_BASE main
        set -x lC_CTYPE en_GB.UTF-8
        set -x LC_ALL en_GB.UTF-8
        set -x LANG en_GB.UTF-8
        set -x CARGO_TARGET_DIR {$HOME}/.cargo-target
        set -x NIXPKGS_ALLOW_UNFREE 1
        set -x NTFY_TOPIC simonrw-notify
        set -x GLAMOUR_STYLE ${glamour-style}

        # centralise where python puts its .pyc files
        set -x PYTHONPYCACHEPREFIX {$HOME}/.python-cache

        set -x EDITOR "nvim";
        set -x PAGER "${pkgs.bat}/bin/bat"

        # host-specific configuration
        set host_config ~/.config/fish/conf.d/per-host/config.(hostname).fish
        test -r $host_config; and source $host_config
        set -e host_config

        function fs
            tmux-session-history
        end

        # tmux configuration
        function _not_inside_tmux
            test -z "$TMUX"
        end

        function _not_inside_neovim
            test -z "$NVIM"
        end

        function _not_inside_emacs
            test -z "$INSIDE_EMACS" && test -z "$EMACS"
        end

        function _not_inside_vscode_term
            test "$TERM_PROGRAM" != "vscode"
        end

        function _not_inside_zellij
            test -z "$ZELLIJ_SESSION_NAME"
        end

        function _not_inside_pycharm
          test -z "$INSIDE_PYCHARM"
        end

        function _inside_x_session
            switch (uname)
                case Linux
                    ps aux | grep -q -i xorg
                case '*'
                    return 0
            end
        end

        function _not_disabled
          test -z "$TMUX_DISABLED"
        end

        function ensure_tmux_is_running
            if _not_disabled && _not_inside_tmux && _not_inside_neovim && _not_inside_emacs && _inside_x_session && _not_inside_vscode_term && _not_inside_zellij && _not_inside_pycharm
                tat
            end
        end

        fish_ssh_agent

        ensure_tmux_is_running

        switch (uname)
            case Darwin
                # set -x DYLD_LIBRARY_PATH {$BUILD_PREFIX}/lib {$DYLD_LIBRARY_PATH}
                #
                # fix nix path
                set -x PATH /etc/profiles/per-user/(whoami)/bin {$PATH} /opt/homebrew/bin
            case '*'
        end

      # fix gpg tty
      if isatty
        set -x GPG_TTY (tty)
      end

      # wrap tflocal completion
      complete --command tflocal --wraps terraform

      # wrap nix completion
      complete --command nom --wraps nix

      # if a local configuration override file exists, source it
      if test -f $HOME/.config/fish/local.fish;
        source $HOME/.config/fish/local.fish
      end
    '';
    shellAliases =
      {
        add-keys = ''ssh-add (find ~/.ssh - maxdepth 1 - type f - name "id_rsa*" | grep - v pub | grep - v bak)'';
        cat = "${pkgs.bat}/bin/bat";
        clear-pycs = "find { $PWD } -name '*.pyc' -delete";
        da = "${pkgs.direnv}/bin/direnv allow";
        de = "${pkgs.direnv}/bin/direnv edit";
        es = ''exec $SHELL'';
        gpe = "${pkgs.git}/bin/git push && exit";
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
        vup = "nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'";
        vimdiff = "nvim -d";
        vim = "nvim";
      }
      // lib.optionalAttrs isLinux {
        pbcopy = "${pkgs.xclip}/bin/xclip -selection clipboard";
        pbpaste = "${pkgs.xclip}/bin/xclip -selection clipboard -o";
      };
    shellAbbrs = {
      c = "cargo";
      d = "dev";
      gco = "git checkout";
      g = "git";
      gp = "git pull";
      ipy = "ipython";
      k = "kubectl";
      nd = "nix develop --command fish";
      n = "notify-wrapper";
      py = "python";
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
        body = ''
          set -l last_pipestatus $pipestatus
          set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
          set -l normal (set_color normal)
          set -q fish_color_status
          or set -g fish_color_status --background=red white

          # Color the prompt differently when we're root
          set -l color_cwd $fish_color_cwd
          set -l suffix '$'
          if functions -q fish_is_root_user; and fish_is_root_user
              if set -q fish_color_cwd_root
                  set color_cwd $fish_color_cwd_root
              end
              set suffix '#'
          end

          # Write pipestatus
          # If the status was carried over (e.g. after `set`), don't bold it.
          set -l bold_flag --bold
          set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
          if test $__fish_prompt_status_generation = $status_generation
              set bold_flag
          end
          set __fish_prompt_status_generation $status_generation
          set -l status_color (set_color $fish_color_status)
          set -l statusb_color (set_color $bold_flag $fish_color_status)
          set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

          set -l num_bg_jobs (count (jobs))
          set -l suffix_color \'\'
          if test $__fish_last_status = 0
              if test $num_bg_jobs = 0
                  set suffix_color (set_color green)
              else
                  set suffix_color (set_color -u green)
              end
          else
              if test $num_bg_jobs = 0
                  set suffix_color (set_color red)
              else
                  set suffix_color (set_color -u red)
              end
          end

          echo
          echo -n -s {$nix_shell_str} {$suffix_color} {$suffix} {$normal} " "
        '';
      };
    };
  };
}
