{ ... }:
{
  services.syncthing = {
    enable = true;
    dataDir = "/home/simon/Documents";
    configDir = "/home/simon/.config/syncthing";
    overrideDevices = true;
    overrideFolders = true;
    openDefaultPorts = true;
    user = "simon";
    group = "users";
    devices = {
      "pixel4a" = {
        id = "R6UM63V-JQOAB66-FCKVP2D-MX22FY3-FT64JSX-CSGPQAG-SJCE76P-TM5VOQY";
      };
      "mba" = {
        id = "CDTTLDZ-VZEOURA-WGOQOX3-SEG62EL-MII4BIH-XZLBCEE-JDTP2AF-WHDXLQK";
      };
    };
    folders = {
      "note" = {
        id = "ma3kn-aj26t";
        path = "/home/simon/notes";
        devices = [
          "mba"
          "pixel4a"
        ];
      };
    };
  };
}
