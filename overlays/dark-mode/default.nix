{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation rec {
  pname = "dark-mode";
  version = "3.0.2";

  src = fetchFromGitHub {
    owner = "sindresorhus";
    repo = "dark-mode";
    rev = "v${version}";
    sha256 = "sha256-vgk26fXrtICYyjxsAomVsgr+iEf3ca3U+KRyXF0HxTM=";
  };

  buildPhase = ''
	LD=$CC /usr/bin/xcodebuild -derivedDataPath $(mktemp -d) -scheme dark-mode OBJROOT=.build SYMROOT=.build
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp .build/Debug/dark-mode $out/bin/
  '';

}
