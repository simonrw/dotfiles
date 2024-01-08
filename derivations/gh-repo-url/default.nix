{pkgs ? import <nixpkgs> {}}:
pkgs.stdenvNoCC.mkDerivation {
  pname = "gh-repo-url";
  version = "unstable";
  src = ./.;
  doBuild = false;
  installPhase = ''
    install -Dm755 ./gh-repo-url $out/bin/gh-repo-url
  '';
}
