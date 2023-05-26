{ pkgs, isLinux, ... }:
let
  custom-extensions = pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "theme-monokai-pro-vscode";
      publisher = "monokai";
      version = "1.2.1";
      sha256 = "sha256-tRMuAqI6zqjvOCoESbJfD4fjgnA93pQ06ppvPDuwceQ=";
    }
    {
      name = "nix-extension-pack";
      publisher = "pinage404";
      version = "3.0.0";
      sha256 = "sha256-cWXd6AlyxBroZF+cXZzzWZbYPDuOqwCZIK67cEP5sNk=";
    }
    {
      name = "vscode-test-explorer";
      publisher = "hbenl";
      version = "2.21.1";
      sha256 = "sha256-fHyePd8fYPt7zPHBGiVmd8fRx+IM3/cSBCyiI/C0VAg=";
    }
    {
      name = "vscode-python-test-adapter";
      publisher = "littlefoxteam";
      version = "0.7.1";
      sha256 = "sha256-XnFgQTXU7h8Po9pI8oeKfzHHpz+dyDff0RkqTyaKS+o=";
    }
    {
      name = "test-adapter-converter";
      publisher = "ms-vscode";
      version = "0.1.7";
      sha256 = "sha256-W5t0X8LwpI6ZL7sx6edo/0jy/ujHHZU+xZWR4f7sbec=";
    }
    {
      name = "remote-containers";
      publisher = "ms-vscode-remote";
      version = "0.292.0";
      sha256 = "sha256-U1ZuxfoUBWdnOy+t4Zp7B5jyvGt89xsufrqnX09gm4U=";
    }
  ];
in
{
  programs.vscode = {
    enable = isLinux;
    extensions = with pkgs.vscode-extensions; [
      arrterian.nix-env-selector
      bungcip.better-toml
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
    ] ++ custom-extensions;
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
