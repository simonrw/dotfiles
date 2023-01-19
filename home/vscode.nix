{ config, pkgs, isLinux, lib, ... }:
{
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      arrterian.nix-env-selector
      bbenoist.nix
      dracula-theme.theme-dracula
      github.github-vscode-theme
      haskell.haskell
      jnoortheen.nix-ide
      justusadam.language-haskell
      matklad.rust-analyzer
      ms-python.vscode-pylance
      ms-vscode-remote.remote-ssh
      vscodevim.vim
    ] ++ lib.optionals isLinux [
      ms-python.python
    ];
    enableExtensionUpdateCheck = true;
    enableUpdateCheck = true;
    mutableExtensionsDir = true;
    keybindings =
      [
        {
          "key" = "ctrl+alt+t";
          "command" = "go.test.workspace";
        }
        {
          "key" = "ctrl+shift+c";
          "command" = "-workbench.action.terminal.openNativeConsole";
          "when" = "!terminalFocus";
        }
        {
          "key" = "ctrl+shift+c";
          "command" = "-workbench.action.terminal.copySelection";
          "when" = "terminalFocus && terminalTextSelected";
        }
        {
          "key" = "ctrl+'";
          "command" = "-unity-tools.OpenDocs";
          "when" = "editorTextFocus && editorLangId == 'csharp'";
        }
        {
          "key" = "ctrl+enter";
          "command" = "-jupyter.runcurrentcell";
          "when" = "editorTextFocus && jupyter.hascodecells && !editorHasSelection && !notebookEditorFocused";
        }
        {
          "key" = "shift+enter";
          "command" = "-python.execSelectionInTerminal";
          "when" = "editorTextFocus && !findInputFocussed && !jupyter.ownsSelection && !replaceInputFocussed && editorLangId == 'python'";
        }
        {
          "key" = "shift+enter";
          "command" = "-python.execSelectionInTerminal";
          "when" = "editorTextFocus && !findInputFocussed && !jupyter.ownsSelection && !notebookEditorFocused && !replaceInputFocussed && editorLangId == 'python'";
        }
        {
          "key" = "ctrl+p";
          "command" = "workbench.action.quickOpen";
          "when" = "editorTextFocus";
        }
        {
          "key" = "ctrl+p";
          "command" = "-workbench.action.quickOpen";
        }
        {
          "key" = "ctrl+e";
          "command" = "workbench.action.quickOpen";
          "when" = "editorTextFocus";
        }
        {
          "key" = "ctrl+e";
          "command" = "-workbench.action.quickOpen";
        }
        {
          "key" = "ctrl+p";
          "command" = "-workbench.action.quickOpenNavigateNextInFilePicker";
          "when" = "inFilesPicker && inQuickOpen";
        }
        {
          "key" = "ctrl+f";
          "command" = "-workbench.action.terminal.focusFind";
          "when" = "terminalFindFocused && terminalProcessSupported || terminalFocus && terminalProcessSupported";
        }
        {
          "key" = "alt+m";
          "command" = "workbench.action.toggleMaximizedPanel";
        }
        {
          "key" = "ctrl+shift+`";
          "command" = "-workbench.action.terminal.new";
          "when" = "terminalProcessSupported";
        }
        {
          "key" = "ctrl+`";
          "command" = "-workbench.action.terminal.toggleTerminal";
          "when" = "terminal.active";
        }
        {
          "key" = "ctrl+'";
          "command" = "workbench.action.terminal.toggleTerminal";
        }
      ];
    userSettings = {
      "git.verboseCommit" = true;
      "nix.enableLanguageServer" = true;
      "editor.lineNumbers" = "off";
      "git.enableSmartCommit" = true;
      "python.formatting.provider" = "black";
      "editor.minimap.enabled" = false;
      "intelephense.telemetry.enabled" = false;
      "jupyter.sendSelectionToInteractiveWindow" = false;
      "explorer.confirmDragAndDrop" = false;
      "editor.autoClosingQuotes" = "never";
      "editor.autoClosingOvertype" = "never";
      "editor.autoClosingBrackets" = "never";
      "explorer.confirmDelete" = false;
      "python.languageServer" = "Pylance";
      # Custom tags for the parser to use
      "workbench.startupEditor" = "none";
      "[rust]" = {
        "editor.defaultFormatter" = "rust-lang.rust-analyzer";
      };
      "workbench.colorCustomizations" = {
        "[Atom One Light]" = { };
        "[Default Dark+]" = { };
      };
      "breadcrumbs.enabled" = true;
      "go.useLanguageServer" = true;
      "omnisharp.path" = "latest";
      "extensions.ignoreRecommendations" = false;
      "[markdown]" = {
        "editor.wordWrap" = "on";
        "editor.quickSuggestions" = {
          "comments" = "off";
          "strings" = "off";
          "other" = "off";
        };
      };
      "[yaml]" = {
        "editor.quickSuggestions" = {
          "comments" = "off";
          "strings" = "off";
          "other" = "off";
        };
      };
      "[git-commit]" = {
        "editor.rulers" = [ 72 ];
        "workbench.editor.restoreViewState" = false;
        "editor.wordWrap" = "wordWrapColumn";
        "editor.wordWrapColumn" = 72;
      };
      "github.gitAuthentication" = false;
      "vim.leader" = ",\\";
      "vim.statusBarColors.visualline" = "";
      "markdown.preview.scrollEditorWithPreview" = false;
      "editor.cursorBlinking" = "solid";
      "python.formatting.blackPath" = "~/.local/pipx/bin/black";
      "python.linting.mypyPath" = "~/.local/pipx/bin/mypy";
      "workbench.editorAssociations" = {
        "*.ipynb" = "jupyter.notebook.ipynb";
        "git-rebase-todo" = "gitlens.rebase";
      };
      "pythonTestExplorer.testFramework" = "pytest";
      "diffEditor.ignoreTrimWhitespace" = false;
      "go.toolsManagement.autoUpdate" = true;
      "rust.rustup" = {
        "toolchain" = "stable-x86_64-unknown-linux-gnu";
      };
      "terminal.integrated.fontSize" = 14;
      "redhat.telemetry.enabled" = false;
      "html.autoClosingTags" = false;
      "liveServer.settings.donotShowInfoMsg" = true;
      "security.workspace.trust.untrustedFiles" = "open";
      "git.confirmSync" = false;
      "python.sortImports.path" = "/Users/swalker/.local/pipx/bin/isort";
      "python.analysis.diagnosticMode" = "workspace";
      "python.analysis.typeCheckingMode" = "basic";
      "python.testing.pytestArgs" = [ "--no-cov" ];
      "explorer.openEditors.visible" = 0;
      "search.quickOpen.includeHistory" = false;
      "terminal.integrated.defaultProfile.osx" = "fish";
      "json.schemas" = [ ];
      "aws.telemetry" = false;
      "debug.allowBreakpointsEverywhere" = true;
      "telemetry.telemetryLevel" = "crash";
      "editor.acceptSuggestionOnEnter" = "off";
      "editor.formatOnSave" = true;
      "files.hotExit" = "off";
      "errorLens.enabledDiagnosticLevels" = [ "error" "warning" ];
      "markdown-preview-github-styles.colorTheme" = "light";
      "remote.SSH.remotePlatform" = {
        "astoria" = "linux";
      };
      "gitlens.hovers.currentLine.over" = "line";
      "gitlens.statusBar.enabled" = false;
      "gitlens.currentLine.enabled" = false;
      "gitlens.codeLens.enabled" = false;
      "syntax.highlightLanguages" = [
        "c"
        "cpp"
        "python"
        "typescript"
        "typescriptreact"
        "javascript"
        "go"
        "rust"
        "php"
        "ruby"
        "shellscript"
        "ocaml"
        "lua"
      ];
      "oneDarkPro.italic" = false;
      "cmake.configureOnOpen" = true;
      "C_Cpp.formatting" = "Disabled";
      "editor.autoClosingDelete" = "never";
      "rust-analyzer.diagnostics.experimental.enable" = true;
      "rust-analyzer.checkOnSave.command" = "clippy";
      "editor.suggest.preview" = true;
      "editor.suggest.showMethods" = true;
      "editor.semanticHighlighting.enabled" = true;
      "[javascript]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "[javascriptreact]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "terminal.integrated.defaultProfile.linux" = "";
      "[typescript]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "workbench.iconTheme" = "Monokai Pro Icons";
      "workbench.sideBar.location" = "right";
      "workbench.colorTheme" = if config.dark-mode then "GitHub Dark" else "Default Light+";
      "C_Cpp.intelliSenseEngine" = "disabled";
      "[jsonc]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      "haskell.manageHLS" = "PATH";
      "window.zoomLevel" = 1;
    };
  };
}
