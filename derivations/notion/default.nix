{
  makeDesktopItem,
  appimageTools,
  fetchurl,
  symlinkJoin,
  ...
}: let
  desktopItem = makeDesktopItem rec {
    name = "Notion";
    exec = "notion";
    icon = "notion";
    desktopName = name;
    genericName = name;
    categories = [
      "System"
    ];
    mimeTypes = [
      "x-scheme-handler/notion"
    ];
  };
  notion = appimageTools.wrapType2 rec {
    name = "notion";
    version = "2.3.2-1";

    src = fetchurl {
      url = "https://github.com/kidonng/notion-appimage/releases/download/${version}/Notion-${version}-x86-64.AppImage";
      hash = "sha256-/nVOjzcO/2fBgYQgLlA2mQimmvHLUGWnpPq3+wsqcRo=";
    };
  };
in
  symlinkJoin {
    name = "notion";
    paths = [
      notion
      desktopItem
    ];
  }
