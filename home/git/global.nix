{
  config,
  pkgs,
  isLinux,
  ...
}: {
  programs.git = {
    enable = true;
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

      pushf = "push --force-with-lease";

      # reset $REVIEW_BASE to upstream
      sync = ''!f() { git fetch origin && git branch -f "$REVIEW_BASE" origin/"$REVIEW_BASE"; }; f'';

      # Taken from http://www.theodo.fr/blog/2017/06/git-game-advanced-git-aliases/
      # Ignore whatever is passed
      ignore = "!f() { echo \"$1\" >> .gitignore; }; f";

      fo = "fetch origin";

      # review shortcuts
      # https://blog.jez.io/cli-code-review
      files = "!git diff --name-only $(git base-commit)";
      stat = "!git diff --stat $(git base-commit)";
      review = "!REVIEW_BASE=$(git base-commit) nvim -c 'set nosplitright' -p $(git files) -c \"tabdo Gvdiff $REVIEW_BASE\" -c 'set splitright'";
      base-commit = "!git merge-base HEAD $REVIEW_BASE";
      review-commits = "!nvim -c 'Gclog --reverse $REVIEW_BASE..'";
      log-base = "!git l $REVIEW_BASE..";

      # ignore modifications to files
      ignore-modifications = "update-index --skip-worktree --";
      reset-ignore-modifications = "!f() { git list-ignored-modifications | xargs git update-index --no-skip-worktree; }; f";
      list-ignored-modifications = "!f() { git ls-files -v | grep '^S' | cut -f 2 -d ' '; }; f";

      # Other aliases
      ff = "!f() { git merge --ff-only origin/$(git branch-name); }; f";
      st = "status";
      co = "checkout";
      su = "submodule update --init --recursive";
      pr = "pull --rebase --prune";
      mergeff = "merge --ff-only";
      ci = "commit -v";
      edit = "!vim `git ls-files -m`";
      cleanup = "!git rebase -i $REVIEW_BASE";
      # Quickly diff the upstream remote (assume origin)
      upstream = "rev-parse --abbrev-ref --symbolic-full-name '@{u}'";
      diffup = "!git diff $(git upstream)..";
      fetchup = "fetch upstream";
      upsub = "submodule foreach 'git checkout master && git pull'";
      authors = "shortlog -s -n --no-merges";
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
      diff-base = "!git diff $(git base-commit)";
      cleanup-base = "!git rebase -i $(git base-commit)";
      # Logging from Gary Bernhardt
      l = "log --graph --decorate --pretty=format:'%C(auto)%h%C(reset) %C(green)(%ar)%C(reset) %C(blue)[%an]%C(auto) %d %s%C(auto)' --exclude='refs/bugs/*' --exclude='refs/identities/*'";
      la = "l --all";
      lm = "!git l $(git branch-name) $REVIEW_BASE";
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
        diff-so-fancy = true;
        side-by-side = true;
      };
    };
    extraConfig = {
      "url \"git@github.com:\"" = {
        pushInsteadOf = "https://github.com";
      };
      branch = {
        autosetuprebase = "always";
        sort = "-committerdate";
      };
      commit = {
        verbose = true;
      };
      rebase = {
        autosquash = true;
        autostash = true;
        updateRefs = true;
      };
      column.ui = "auto";
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
        algorithm = "histogram";
        tool = "meld";
        colormoved = "default";
        colormovedws = "allow-indentation-change";
      };
      difftool.prompt = false;
      difftool.meld = {
        cmd = "${pkgs.meld}/bin/meld $LOCAL $REMOTE";
      };
      mergetool = {
        smerge = {
          # smerbe is installed via homebrew on macos
          cmd =
            if isLinux
            then ''${pkgs.sublime-merge}/bin/smerge''
            else "smerge";
          keepBackup = false;
        };
        pycharm = {
          cmd = ''pycharm-community merge "$LOCAL" "$REMOTE" "$BASE" "$MERGED"'';
          keepBackup = false;
        };
        diffview = {
          cmd = "nvim -c DiffviewOpen";
          keepBackup = false;
        };
        conflicted = {
          cmd = "nvim +Conflicted";
          keepBackup = false;
        };
        prompt = false;
        vimdiff = {
          cmd = "nvim -d";
          keepBackup = false;
        };
        meld = {
          cmd = ''${pkgs.meld}/bin/meld "$LOCAL" "$MERGED" "$REMOTE" --output "$MERGED"'';
          keepBackup = false;
        };
      };
      github = {
        user = "simonrw";
      };
      rerere = {
        enabled = true;
        autoUpdate = true;
      };
      push = {
        default = "current";
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
        tool = "diffview";
        conflictstyle = "zdiff3";
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
      ".env"
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

  programs.git-cliff = {
    enable = true;
  };
}
