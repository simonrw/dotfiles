{
  stdenv,
  fetchurl,
  autoPatchelfHook,
  libusb1,
  webkitgtk,
  gtk3-x11,
}:
stdenv.mkDerivation rec {
  pname = "wally";
  version = "2.1.3-linux";

  src = fetchurl {
    url = "https://github.com/zsa/wally/releases/download/${version}/wally";
    sha256 = "sha256-owyXTC/VRJdeSPfyrJmiH5Nvo+CAOv7rEJaCanmv294=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
  ];

  buildInputs = [
    libusb1
    webkitgtk
    gtk3-x11
  ];

  dontUnpack = true;
  doBuild = false;

  installPhase = ''
    install -Dm755 $src $out/bin/wally
  '';
}
