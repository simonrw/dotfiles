{ config, lib, pkgs, ... }:
{
  programs.helix = {
    enable = true;
    settings = {
      editor = {
        auto-pairs = false;
        true-color = true;
        lsp.display-messages = true;
        gutters.layout = [
          "diagnostics"
          "spacer"
          "diff"
        ];
      };
      keys.normal = {
        "G" = "goto_file_end";
        "g" = {
          "q" = ":reflow";
        };
        "space" = {
          "q" = ":quit";
          "w" = ":write";
        };
      };
      language =
        [
          {
            name = "python";
            language-server = {
              command = "${pkgs.pyright}/bin/pyright-langserver";
              args = [
                "--stdio"
              ];
            };
            config = { };
          }
          {
            name = "nix";
            language-server = {
              command = "${lib.getExe pkgs.rnix-lsp}";
            };
          }
        ];
    };
    themes = {
      monokai-pro-custom = {
        inherits = "monokai_pro";
        comment = "#e69340";
      };
      srw =
        let
          my_black = "#111111";       # Cursorline
          my_gray0 = "#07151a";       # Default Background
          my_gray1 = "#0e0e0e";       # Ruler
          my_gray2 = "#1a1a1a";       # Lighter Background (Used for status bars, line number and folding marks)
          my_gray3 = "#323232";       # Selection Background
          my_gray4 = "#7c7c7c";       # Comments, Invisibles, Line Highlighting
          my_gray5 = "#aaaaaa";       # Dark Foreground (Used for status bars)
          my_gray6 = "#c0c0c0";       # Light Foreground (Not often used)
          my_white = "#F3F2CC";       # Default Foreground, Caret, Delimiters, Operators
          my_white2 = "#F3F2CC";      # Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
          my_white3 = "#F3F2CC";      # Classes, Markup Bold, Search Text Background
          my_turquoise = "#86c1b9";   # Support, Regular Expressions, Escape Characters
          my_turquoise2 = "#72a59e";  # URL
          my_green = "#99be70";       # Strings, Inherited Class, Markup Code, Diff Inserted
          my_brown = "#cfba8b";       # Member variables, Quotes
          my_yellow1 = "#FAD566";     # Functions, Methods, Attribute IDs, Headings
          my_yellow2 = "#ffff9f";     # Debug, Info
          my_red = "#F05E48";         # Keywords, Storage, Selector, Diff Changed
        in
        {
          "ui.background" = { bg = my_gray0; };
          "ui.menu" = { fg = my_white; bg = my_gray2; };
          "ui.menu.selected" = { fg = my_gray2; bg = my_gray5; };
          "ui.linenr" = { fg = my_gray4; bg = my_gray2; };
          "ui.popup" = { bg = my_gray2; };
          "ui.window" = { fg = my_gray4; bg = my_gray2; };
          "ui.linenr.selected" = { fg = my_gray6; bg = my_gray1; };
          "ui.selection" = { bg = my_gray3; };
          "comment" = { fg = my_gray4; bg = my_gray0; };
          "ui.cursorline" = { bg = my_gray2; };
          "ui.statusline" = { fg = my_gray6; bg = my_gray2; };
          "ui.statusline.inactive" = { fg = my_gray4; bg = my_gray2; };
          "ui.statusline.insert" = { fg = my_black; bg = my_gray5; modifiers = [ "bold" ]; };
          "ui.statusline.normal" = { fg = my_gray6; bg = my_gray2; };
          "ui.statusline.select" = { fg = my_gray6; bg = my_black; modifiers = [ "bold" ]; };
          "ui.cursor" = { fg = my_gray5; modifiers = [ "reversed" ]; };
          "ui.cursor.primary" = { fg = my_white; modifiers = [ "reversed" ]; };
          "ui.cursorline.primary" = { bg = my_black; };
          "ui.cursorline.secondary" = { bg = my_black; };
          "ui.text" = my_white;
          "operator" = my_white;
          "ui.text.focus" = my_white;
          "variable" = my_white3;
          "constant.numeric" = my_turquoise;
          "constant" = my_white3;
          "attribute" = my_turquoise;
          "type" = { fg = my_white3; modifiers = [ "italic" ]; };
          "ui.cursor.match" = { fg = my_white3; modifiers = [ "underlined" ]; };
          "string" = my_green;
          "variable.other.member" = my_brown;
          "constant.character.escape" = my_turquoise;
          "function" = my_yellow1;
          "constructor" = my_yellow1;
          "special" = my_yellow1;
          "keyword" = my_red;
          "label" = my_red;
          "namespace" = my_white3;
          "ui.help" = { fg = my_gray6; bg = my_gray2; };
          "ui.virtual.whitespace" = { fg = my_gray5; };
          "ui.virtual.ruler" = { bg = my_gray1; };

          "markup.heading" = my_yellow1;
          "markup.list" = my_white2;
          "markup.bold" = { modifiers = [ "bold" ]; };
          "markup.italic" = { modifiers = [ "italic" ]; };
          "markup.link.url" = my_turquoise2;
          "markup.link.text" = my_white2;
          "markup.quote" = my_brown;
          "markup.raw" = my_green;

          "diff.plus" = my_green;
          "diff.delta" = my_white;
          "diff.minus" = my_red;

          "diagnostic" = { modifiers = [ "underlined" ]; };
          "ui.gutter" = { bg = my_gray2; };
          "hint" = my_gray5;
          "debug" = my_yellow2;
          "info" = my_yellow2;
          "warning" = my_yellow2;
          "error" = my_red;
        };
    };
  };
}
