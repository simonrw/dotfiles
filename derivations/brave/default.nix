{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs) lib system fetchFromGitHub stdenv fetchurl;
in
stdenv.mkDerivation rec {
  pname = "brave";
  version = "1.45.108";

  buildInputs = with pkgs; [
    cpio
    xar
  ];

  src = fetchurl {
    url = "https://github.com/brave/brave-browser/releases/download/v${version}/Brave-Browser-universal.pkg";
    sha256 = "44a700f70cdfe765a66884d65fa1d20b1124bfb0aadbb933d3ef9e5ff5ed1589";
  };

  unpackPhase = ''
    xar -xf $src
  '';

  buildPhase = ''
    cat Payload | gunzip -dc | cpio -i
  '';

  installPhase = ''
    mkdir -p $out/Applications
    cp -r 'Brave Browser.app' $out/Applications
  '';

  meta = with lib; {
    homepage = "https://brave.com";
    description = "Next generation Brave browser for Android, Linux, macOS, Windows";
    license = licenses.mpl20;
    maintainers = with maintainers; [ ];
  };
}
