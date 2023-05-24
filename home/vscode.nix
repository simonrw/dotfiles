{ pkgs, isLinux, ... }:
{
  programs.vscode = {
    enable = isLinux;
    package = pkgs.vscode.fhsWithPackages (ps: with ps; [
      rustup
      zlib
      openssl.dev
      pkg-config
    ]);
    mutableExtensionsDir = true;
    userSettings = {
  "workbench.colorTheme"= "Monokai Pro";
  "editor.minimap.enabled"= false;
  "telemetry.telemetryLevel"= "off";
  "editor.autoClosingBrackets"= "never";
  "editor.autoClosingQuotes"= "never";
  "editor.autoClosingOvertype"= "never";
  "rust-analyzer.check.command"= "clippy";
  "editor.formatOnSave"= true;
  "git.verboseCommit"= true;
  "git.enableSmartCommit"= true;
  "python.formatting.provider"= "black";
  "explorer.confirmDragAndDrop"= false;
  "python.languageServer"= "Pylance";
  "workbench.startupEditor"= "none";
  "[rust]"= {
    "editor.defaultFormatter"= "rust-lang.rust-analyzer";
  };
  "breadcrumbs.enabled"= true;
  "go.useLanguageServer"= true;
  "[git-commit]"= {
    "editor.rulers"= [72];
    "workbench.editor.restoreViewState"= false;
  };
  "vim.statusBarColors.visualline"= "";
  "editor.cursorBlinking"= "solid";
  "security.workspace.trust.untrustedFiles"= "open";
  "python.analysis.diagnosticMode"= "workspace";
  "python.analysis.typeCheckingMode"= "basic";
  "python.testing.pytestArgs"= ["--no-cov"];
  "debug.allowBreakpointsEverywhere"= true;
  "editor.acceptSuggestionOnEnter"= "off";
  "editor.autoClosingDelete"= "never";
  "editor.suggest.preview"= true;
  "editor.suggest.showMethods"= true;
  "editor.semanticHighlighting.enabled"= true;
  "workbench.editor.showTabs"= false;
  "diffEditor.ignoreTrimWhitespace"= false;
  "editor.lineNumbers"= "off";
  "githubPullRequests.pullBranch"= "never";
  "svelte.enable-ts-plugin"= true;
  "python.defaultInterpreterPath"= "/home/simon/work/localstack/scratch/s3-putobject-repro/.venv";
  "window.menuBarVisibility"= "toggle";
  "workbench.iconTheme"= "Monokai Pro Icons";
    };
  };
}
