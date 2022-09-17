{ config, pkgs, lib, ... }:
let
  hammerspoonDerivation = (import ./derivations/hammerspoon.nix) { inherit pkgs; };
in
{
  home = {
    username = "simon";
    homeDirectory = "/Users/simon";
    stateVersion = "22.05";

    sessionVariables = {
      LANG = "en_GB.UTF-8";
      LC_CTYPE = "en_GB.UTF-8";
      LC_ALL = "en_GB.UTF-8";
      EDITOR = "nvim";
      PAGER = "bat";
      MANPAGER = "sh -c 'col -bx | ${pkgs.bat}/bin/bat -l man -p'";
    };

    packages = with pkgs; [
      bat
      curl
      entr
      exa
      fd
      fzf
      graphviz
      hey
      htop
      httpie
      mkcert
      multitail
      noti
      openssh
      # why does this compile a version of nodejs from scratch?
      pre-commit
      python3
      python3Packages.pipx
      ripgrep
      skim
      universal-ctags

      # custom derivations
      # hammerspoonDerivation
    ] ++ (lib.optionals stdenv.isDarwin [
      # macos only
      reattach-to-user-namespace
    ]);
  };

  programs.bat = {
    enable = true;

    config = {
      theme = "Monokai Extended";
      style = "plain";
    };
  };

  programs.home-manager.enable = true;
  programs.jq.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
    stdlib = builtins.readFile ./direnv/direnvrc;
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
        set -x SHELL (command -v fish)

        set -x BUILD_PREFIX {$HOME}/.local
        set -x PATH {$BUILD_PREFIX}/bin {$HOME}/.bin {$HOME}/.poetry/bin /usr/local/bin {$HOME}/.cargo/bin {$HOME}/bin {$GOPATH}/bin {$PATH}
        set -x GOPATH {$HOME}/dev/gocode
        set -x EDITOR nvim
        set -x REVIEW_BASE main
        set -x PYTEST_ADDOPTS "-p no:sugar"
        set -x LANG en_GB.UTF-8
        set -x CARGO_TARGET_DIR {$HOME}/.cargo/cache
        set -x NIXPKGS_ALLOW_UNFREE 1

        # experimental: enable cargo sparse registry for faster downloads
        set -x CARGO_UNSTABLE_SPARSE_REGISTRY true

        # host-specific configuration
        set host_config ~/.config/fish/conf.d/per-host/config.(hostname).fish
        test -r $host_config; and source $host_config
        set -e host_config

        # skim configuration
        set -x SKIM_DEFAULT_OPTIONS "--color dark,matched_bg:-1 --tiebreak begin --ansi --no-mouse --tabstop 4 --inline-info"
        set -x SKIM_DEFAULT_COMMAND 'rg --files --no-ignore --hidden --follow -g "!{.git,venv,node_modules}/*" 2> /dev/null'

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

        function _inside_x_session
            switch (uname)
                case Linux
                    ps aux | grep -q -i xorg
                case '*'
                    return 0
            end
        end

        function ensure_tmux_is_running
            if _not_inside_tmux && _not_inside_neovim && _not_inside_emacs && _inside_x_session && _not_inside_vscode_term && _not_inside_zellij
                tat
            end
        end

        fish_ssh_agent

        ensure_tmux_is_running

        switch (uname)
            case Darwin
                # set -x DYLD_LIBRARY_PATH {$BUILD_PREFIX}/lib {$DYLD_LIBRARY_PATH}
  
                if not type -q exa
                    if type -q gls
                        alias ls "gls --color=auto"
                        alias thor "gls -thor"
                    end
                end
            case '*'
        end

      # fix nix path
      set -x PATH /etc/profiles/per-user/(whoami)/bin {$PATH}
    '';
    shellAliases = {
      pbcopy = "xclip";
      pbpaste = "xclip -o";
      ta = "_tmux_attach";
      ll = "ls -lh";
      pylab = "ipython - -pylab";
      clear-pycs = "find { $PWD } -name '*.pyc' -delete";
      es = ''exec $SHELL'';
      sourceenv = "source ./venv/bin/activate";
      vup = "nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'";
      add-keys = ''ssh-add (find ~/.ssh - maxdepth 1 - type f - name "id_rsa*" | grep - v pub | grep - v bak)'';
      gpe = "git push && exit";
      gpr = "git pull --rebase";
      tl = "tmux-last";
      gs = "git status";
      de = "direnv edit";
      da = "direnv allow";
      grep = "rg";
      ls = "exa";
      tree = "exa -T";
      thor = "exa -s modified -l";
      lr = "thor";
      cat = "bat";
      less = "bat";
      more = "bat";
      n = "noti";
      lpb = "glab project view -w";
      mr = "glab mr view -w";
      ci = "glab ci view";
      nr = "nix repl --file '<nixpkgs>'";
    };
    shellAbbrs = {
      ipy = "ipython";
      py = "python";
      g = "git";
      k = "kubectl";
      gp = "git pull";
      gco = "git checkout";
      v = "vim";
      c = "cargo";
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
      fish_greeting = {
        description = "Greeting to show when starting a fish shell";
        body = "";
      };
      mcd = {
        description = "Create and move into new directory";
        body = "mkdir -p $argv[1]; and cd $argv[1]";
      };
      fish_user_key_bindings = {
        body = ''
            # Store current token in $dir as root for the 'find' command
            function skim-file-widget -d "List files and folders"
                set -l commandline (__skim_parse_commandline)
                set -l dir $commandline[1]
                set -l skim_query $commandline[2]

                # "-path \$dir'*/\\.*'" matches hidden files/folders inside $dir but not
                # $dir itself, even if hidden.
                test -n "$SKIM_CTRL_T_COMMAND"; or set -l SKIM_CTRL_T_COMMAND "
                command find -L \$dir -mindepth 1 \\( -path \$dir'*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' \\) -prune \
                -o -type f -print \
                -o -type d -print \
                -o -type l -print 2> /dev/null | sed 's@^\./@@'"

                test -n "$SKIM_TMUX_HEIGHT"; or set SKIM_TMUX_HEIGHT 40%
                begin
                set -lx SKIM_DEFAULT_OPTIONS "--height $SKIM_TMUX_HEIGHT --reverse $SKIM_DEFAULT_OPTIONS $SKIM_CTRL_T_OPTS"
                eval "$SKIM_CTRL_T_COMMAND | "(__skimcmd)' -m --query "'$skim_query'"' | while read -l r; set result $result $r; end
                end
                if [ -z "$result" ]
                commandline -f repaint
                return
                else
                # Remove last token from commandline.
                commandline -t ""
                end
                for i in $result
                commandline -it -- (string escape $i)
                commandline -it -- ' '
                end
                commandline -f repaint
            end

            function skim-history-widget -d "Show command history"
                test -n "$SKIM_TMUX_HEIGHT"; or set SKIM_TMUX_HEIGHT 40%
                begin
                set -lx SKIM_DEFAULT_OPTIONS "--height $SKIM_TMUX_HEIGHT $SKIM_DEFAULT_OPTIONS --tiebreak=index --bind=ctrl-r:toggle-sort $SKIM_CTRL_R_OPTS --no-multi"

                set -l FISH_MAJOR (echo $version | cut -f1 -d.)
                set -l FISH_MINOR (echo $version | cut -f2 -d.)

                # history's -z flag is needed for multi-line support.
                # history's -z flag was added in fish 2.4.0, so don't use it for versions
                # before 2.4.0.
                if [ "$FISH_MAJOR" -gt 2 -o \( "$FISH_MAJOR" -eq 2 -a "$FISH_MINOR" -ge 4 \) ];
                    history -z | eval (__skimcmd) --read0 --print0 -q '(commandline)' | read -lz result
                    and commandline -- $result
                else
                    history | eval (__skimcmd) -q '(commandline)' | read -l result
                    and commandline -- $result
                end
                end
                commandline -f repaint
            end

            function skim-cd-widget -d "Change directory"
                set -l commandline (__skim_parse_commandline)
                set -l dir $commandline[1]
                set -l skim_query $commandline[2]

                test -n "$SKIM_ALT_C_COMMAND"; or set -l SKIM_ALT_C_COMMAND "
                command find -L \$dir -mindepth 1 \\( -path \$dir'*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' \\) -prune \
                -o -type d -print 2> /dev/null | sed 's@^\./@@'"
                test -n "$SKIM_TMUX_HEIGHT"; or set SKIM_TMUX_HEIGHT 40%
                begin
                set -lx SKIM_DEFAULT_OPTIONS "--height $SKIM_TMUX_HEIGHT --reverse $SKIM_DEFAULT_OPTIONS $SKIM_ALT_C_OPTS"
                eval "$SKIM_ALT_C_COMMAND | "(__skimcmd)' --no-multi --query "'$skim_query'"' | read -l result

                if [ -n "$result" ]
                    cd $result

                    # Remove last token from commandline.
                    commandline -t ""
                end
                end

                commandline -f repaint
            end

            function __skimcmd
                test -n "$SKIM_TMUX"; or set SKIM_TMUX 0
                test -n "$SKIM_TMUX_HEIGHT"; or set SKIM_TMUX_HEIGHT 40%
                if [ -n "$SKIM_TMUX_OPTS" ]
                echo "sk-tmux $SKIM_TMUX_OPTS -- "
                else if [ $SKIM_TMUX -eq 1 ]
                echo "sk-tmux -d$SKIM_TMUX_HEIGHT -- "
                else
                echo "sk"
                end
            end

            bind \ct skim-file-widget
            bind \cr skim-history-widget
            bind \ec skim-cd-widget

            if bind -M insert > /dev/null 2>&1
                bind -M insert \ct skim-file-widget
                bind -M insert \cr skim-history-widget
                bind -M insert \ec skim-cd-widget
            end

            function __skim_parse_commandline -d 'Parse the current command line token and return split of existing filepath and rest of token'
                # eval is used to do shell expansion on paths
                set -l commandline (eval "printf '%s' "(commandline -t))

                if [ -z $commandline ]
                # Default to current directory with no --query
                set dir '.'
                set skim_query \'\'
                else
                set dir (__skim_get_dir $commandline)

                if [ "$dir" = "." -a (string sub -l 1 -- $commandline) != '.' ]
                    # if $dir is "." but commandline is not a relative path, this means no file path found
                    set skim_query $commandline
                else
                    # Also remove trailing slash after dir, to "split" input properly
                    set skim_query (string replace -r "^$dir/?" -- \'\' "$commandline")
                end
                end

                echo $dir
                echo $skim_query
            end

            function __skim_get_dir -d 'Find the longest existing filepath from input string'
                set dir $argv

                # Strip all trailing slashes. Ignore if $dir is root dir (/)
                if [ (string length -- $dir) -gt 1 ]
                set dir (string replace -r '/*$' -- \'\' $dir)
          end

          # Iteratively check if dir exists and strip tail end of path
          while [ ! -d "$dir" ]
          # If path is absolute, this can keep going until ends up at /
          # If path is relative, this can keep going until entire input is consumed, dirname returns "."
          set dir (dirname -- "$dir")
          end

          echo $dir
          end
        '';
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
          echo -n -s {$suffix_color} {$suffix} {$normal} " "
        '';
      };
    };
  };

  programs.git = {
    enable = true;
    userName = "Simon Walker";
    userEmail = "s.r.walker101@googlemail.com";
    aliases = {
      # Long form aliases
      # See https://github.com/GitAlias/gitalias/blob/master/gitalias.txt
      publish = "!git push -u origin $(git branch-name)";
      unpublish = "!git push origin :$(git branch-name)";
      branch-name = "!git rev-parse --abbrev-ref HEAD";
      graphviz = "!f() { echo 'digraph git {' ; git log --pretty='format:  %h -> { %p }' \"$@\" | sed 's/[0-9a-f][0-9a-f]*/\"&\"/g' ; echo '}'; }; f";
      trust = "!f() { echo 'marking current directory as safe (includes ./bin on PATH)' && mkdir -p .git/safe; }; f";
      update-commit = "commit --amend --no-edit";
      uc = "update-commit";
      graph = "log --graph --all --decorate --stat --date iso";

      # Taken from http://www.theodo.fr/blog/2017/06/git-game-advanced-git-aliases/
      # Ignore whatever is passed
      ignore = "!f() { echo \"$1\" >> .gitignore; }; f";

      # review shortcuts
      # https://blog.jez.io/cli-code-review/?utm_source=pocket_mylist
      files = "!git diff --name-only $(git merge-base HEAD \"$REVIEW_BASE\")";
      stat = "!git diff --stat $(git merge-base HEAD \"$REVIEW_BASE\")";
      review = "!nvim -p $(git files) +\"tabdo Gvdiff $REVIEW_BASE\"";

      # ignore modifications to files
      ignore-modifications = "update-index --skip-worktree --";
      reset-ignore-modifications = "!f() { git list-ignored-modifications | xargs git update-index --no-skip-worktree; }; f";
      list-ignored-modifications = "!f() { git ls-files -v | grep '^S' | cut -f 2 -d ' '; }; f";

      # Other aliases
      ff = "f() { git merge --ff-only origin/$(git branch-name); }; f";
      st = "status";
      co = "checkout";
      su = "submodule update --init --recursive";
      pr = "pull --rebase --prune";
      mergeff = "merge --ff-only";
      sync = "!git pr && git push";
      ci = "commit -v";
      edit = "!vim `git ls-files -m`";
      cleanup = "!git rebase -i $REVIEW_BASE";
      # Quickly diff the upstream remote (assume origin)
      upstream = "rev-parse --abbrev-ref --symbolic-full-name '@{u}'";
      diffup = "!git diff $(git upstream)..";
      fetchup = "fetch upstream";
      upsub = "submodule foreach 'git checkout master && git pull'";
      authors = "shortlog -s -n --all --no-merges";
      # https://thoughtbot.com/blog/powerful-git-macros-for-automating-everyday-workflows
      branches = "for-each-ref --sort=-committerdate --format=\"%(color:blue)%(authordate:relative)\t%(color:red)%(authorname)\t%(color:white)%(color:bold)%(refname:short)\" refs/remotes";
      tags = "tag";
      stashes = "stash list";
      unstage = "reset -q HEAD --";
      discard = "checkout --";
      amend = "commit --amend -v";
      precommit = "diff --cached --diff-algorithm=minimal -w";
      pre = "precommit";
      # diff between the current commit and the most recent common ancestor to master (mimics gitlab's interface)
      diff-base = "!git diff $(git merge-base $REVIEW_BASE HEAD)";
      cleanup-base = "!git rebase -i $(git merge-base $REVIEW_BASE $(git rev-parse --abbrev-ref HEAD))";
      # Logging from Gary Bernhardt
      l = "log --graph --decorate --pretty=format:'%C(auto)%h%C(reset) %C(green)(%ar)%C(reset) %C(blue)[%an]%C(auto) %d %s%C(auto)' --exclude='refs/bugs/*' --exclude='refs/identities/*'";
      la = "l --all";
      head = "l -1";
      h = "head";
      r = "l -20";
      ol = "log --oneline --decorate --graph";
      ra = "r --all";
      dc = "precommit";
      aa = "add --all";
      aap = "add --all --patch";
      oneline = "log --oneline --decorate";
      today = "diff @{yesterday}..";
      last = "log -1";
      up = "update";
    };
    delta = {
      enable = true;
      options = {
        side-by-side = true;
        diff-so-fancy = false;
      };
    };
    includes = [
      {
        path = "~/.gitlocalconfig/local";
      }
      {
        path = "~/dev/.gitconfig";
        condition = "gitdir:~/dev/";
      }
      {
        path = "~/dev/.gitconfig";
        condition = "gitdir:~/tmp/";
      }
      {
        path = "~/dev/.gitconfig";
        condition = "gitdir:/tmp/";
      }
      {
        path = "~/dev/.gitconfig";
        condition = "gitdir:~/dotfiles/";
      }
      {
        path = "~/work/.gitconfig";
        condition = "gitdir:~/work/";
      }
    ];
    extraConfig = {
      branch = {
        autosetuprebase = "always";
      };
      color = {
        ui = true;
      };
      core = {
        askPass = "";
        abbrev = 12;
        autocrlf = "input";
        safecrlf = true;
        whitespace = "fix";
        editor = "nvim";
        mergeoptions = "--no-ff";
        preloadindex = true;
        ignorecase = false;
      };
      credential = {
        heloer = "cache";
      };
      diff = {
        algorithm = "patience";
        tool = "icdiff";
        colorMoved = "default";
      };
      mergetool.conflicted = {
        cmd = "nvim +Conflicted";
      };
      github = {
        user = "mindriot101";
      };
      push = {
        default = "tracking";
        followTags = true;
        autoSetupRemote = true;
      };
      fetch = {
        prune = 1;
      };
      grep = {
        extendedRegexp = true;
      };
      init = {
        defaultBranch = "main";
      };
      merge = {
        tool = "conflicted";
        conflictstyle = "diff3";
      };
      transfer = {
        fsckobjects = true;
      };
      status = {
        short = 1;
        branch = 1;
        submoduleSummary = true;
      };
    };
    ignores = [
      ".vscode"
      # direnv stuff
      ".direnv"
      ".envrc"
      # Ignore coc.vim local vim dir
      ".vim"
      # In any project, we want to ignore the "scratch" subdirectories
      "scratch"
      # Ignore Session.vim files created by obsession.vim
      "Session.vim"
      # Ignore tags files
      "tags"
      "tags.lock"
      "tags*temp"
      "tags*.tmp"
      "tmp"
      # Created by https://www.gitignore.io/api/linux,macos,windows
      # Edit at https://www.gitignore.io/?templates=linux,macos,windows
      ### Linux ###
      "*~"
      # temporary files which can be created if a process still has a handle open of a deleted file
      ".fuse_hidden*"
      # KDE directory preferences
      ".directory"
      # Linux trash folder which might appear on any partition or disk
      ".Trash-*"
      # .nfs files are created when an open file is removed but is still being accessed
      ".nfs*"
      ### macOS ###
      # General
      ".DS_Store"
      ".AppleDouble"
      ".LSOverride"
      # Icon must end with two \r
      "Icon"
      # Thumbnails
      "._*"
      # Files that might appear in the root of a volume
      ".DocumentRevisions-V100"
      ".fseventsd"
      ".Spotlight-V100"
      ".TemporaryItems"
      ".Trashes"
      ".VolumeIcon.icns"
      ".com.apple.timemachine.donotpresent"
      # Directories potentially created on remote AFP share
      ".AppleDB"
      ".AppleDesktop"
      "Network Trash Folder"
      "Temporary Items"
      ".apdisk"
      ### Windows ###
      # Windows thumbnail cache files
      "Thumbs.db"
      "Thumbs.db:encryptable"
      "ehthumbs.db"
      "ehthumbs_vista.db"
      # Dump file
      "*.stackdump"
      # Folder config file
      "[Dd]esktop.ini"
      # Recycle Bin used on file shares
      "$RECYCLE.BIN/"
      # Windows Installer files
      "*.cab"
      "*.msi"
      "*.msix"
      "*.msm"
      "*.msp"
      # Windows shortcuts
      "*.lnk"
      # End of https://www.gitignore.io/api/linux,macos,windows
      "Session.vim*"
    ];
    attributes = [
      "*.c     diff=cpp"
      "*.h     diff=cpp"
      "*.c++   diff=cpp"
      "*.h++   diff=cpp"
      "*.cpp   diff=cpp"
      "*.hpp   diff=cpp"
      "*.cc    diff=cpp"
      "*.hh    diff=cpp"
      "*.m     diff=objc"
      "*.mm    diff=objc"
      "*.cs    diff=csharp"
      "*.css   diff=css"
      "*.html  diff=html"
      "*.xhtml diff=html"
      "*.ex    diff=elixir"
      "*.exs   diff=elixir"
      "*.go    diff=golang"
      "*.php   diff=php"
      "*.pl    diff=perl"
      "*.py    diff=python"
      "*.md    diff=markdown"
      "*.rb    diff=ruby"
      "*.rake  diff=ruby"
      "*.rs    diff=rust"
      "*.lisp  diff=lisp"
      "*.el    diff=lisp"
    ];
    lfs = {
      enable = true;
    };
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    withPython3 = true;
    extraPython3Packages = (ps: with ps; [
      pynvim
    ]);
  };

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
    terminal = "xterm-256color";
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
        commonFiles = with builtins; [
          (readFile ./tmux/tmux.conf)
          (readFile ./tmux/srw-colourscheme.conf)
        ];
        darwinFiles = lib.optionals stdenv.isDarwin [
          (builtins.readFile ./tmux/tmux-osx.conf)
        ];
      in
      (builtins.concatStringsSep "\n" (commonFiles ++ darwinFiles));
  };

  home.file = {
    ".bin" = {
      source = ./bin;
      recursive = true;
    };
  };

  xdg = {
    enable = true;
    configFile.nvim = {
      source = ./nvim;
      recursive = true;
    };
  };
}
