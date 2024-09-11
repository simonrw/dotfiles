{pkgs ? import <nixpkgs> {}}:
pkgs.stdenvNoCC.mkDerivation {
  pname = "gh-pr-url";
  version = "unstable";
  src = ./.;
  doBuild = false;
  installPhase = ''
    install -Dm755 ./gh-pr-url $out/bin/gh-pr-url
  '';
}
