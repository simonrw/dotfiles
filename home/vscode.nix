{ pkgs, isLinux, ... }:
{
  programs.vscode = {
    enable = isLinux;
    extensions = with pkgs.vscode-extensions; [
      arrterian.nix-env-selector
      bungcip.better-toml
      dracula-theme.theme-dracula
      esbenp.prettier-vscode
      github.github-vscode-theme
      github.vscode-github-actions
      github.vscode-pull-request-github
      golang.go
      jnoortheen.nix-ide
      matklad.rust-analyzer
      mkhl.direnv
      ms-python.python
      ms-python.vscode-pylance
      ms-vscode.makefile-tools
      skellock.just
      svelte.svelte-vscode
      vadimcn.vscode-lldb
      vscodevim.vim
    ] ++ (with pkgs.vscode-marketplace; [
      monokai.theme-monokai-pro-vscode
      pinage404.nix-extension-pack
      hbenl.vscode-test-explorer
      littlefoxteam.vscode-python-test-adapter
      ms-vscode.test-adapter-converter
      ms-vscode-remote.remote-containers
    ]);
    mutableExtensionsDir = false;
    userSettings = {
      "editor.minimap.enabled" = false;
      "telemetry.telemetryLevel" = "off";
      "editor.autoClosingBrackets" = "never";
      "editor.autoClosingQuotes" = "never";
      "editor.autoClosingOvertype" = "never";
      "rust-analyzer.check.command" = "clippy";
      "editor.formatOnSave" = true;
      "git.verboseCommit" = true;
      "git.enableSmartCommit" = true;
      "python.formatting.provider" = "black";
      "explorer.confirmDragAndDrop" = false;
      "python.languageServer" = "Pylance";
      "workbench.startupEditor" = "none";
      "[rust]" = {
        "editor.defaultFormatter" = "rust-lang.rust-analyzer";
      };
      "breadcrumbs.enabled" = true;
      "go.useLanguageServer" = true;
      "[git-commit]" = {
        "editor.rulers" = [ 72 ];
        "workbench.editor.restoreViewState" = false;
      };
      "vim.statusBarColors.visualline" = "";
      "editor.cursorBlinking" = "solid";
      "security.workspace.trust.untrustedFiles" = "open";
      "python.analysis.diagnosticMode" = "workspace";
      "python.analysis.typeCheckingMode" = "basic";
      "python.testing.pytestArgs" = [ "--no-cov" ];
      "debug.allowBreakpointsEverywhere" = true;
      "editor.acceptSuggestionOnEnter" = "off";
      "editor.autoClosingDelete" = "never";
      "editor.suggest.preview" = true;
      "editor.suggest.showMethods" = true;
      "editor.semanticHighlighting.enabled" = true;
      "workbench.editor.showTabs" = false;
      "diffEditor.ignoreTrimWhitespace" = false;
      "editor.lineNumbers" = "off";
      "githubPullRequests.pullBranch" = "never";
      "svelte.enable-ts-plugin" = true;
      "python.defaultInterpreterPath" = "/home/simon/work/localstack/scratch/s3-putobject-repro/.venv";
      "window.menuBarVisibility" = "toggle";
      "workbench.iconTheme" = "Monokai Pro Icons";
    };
  };
}
