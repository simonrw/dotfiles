{pkgs ? import <nixpkgs> {}}:
pkgs.stdenvNoCC.mkDerivation {
  pname = "gh-rebase-pr";
  version = "unstable";
  src = pkgs.fetchFromGitHub {
    owner = "simonrw";
    repo = "gh-rebase-pr";
    rev = "7aecb300e1b1c777e05a1439b01a301209ae99c1";
    hash = "sha256-mWgBOfGhwUxKCdgkGf/kV79os2iFkncfkzFnGNZIuXI=";
  };
  doBuild = false;
  installPhase = ''
    install -Dm755 ./gh-rebase-pr $out/bin/gh-rebase-pr
  '';
}
