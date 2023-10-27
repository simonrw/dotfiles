{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenvNoCC.mkDerivation {
  pname = "gh-rebase-pr";
  version = "unstable";
  src = pkgs.fetchFromGitHub {
    owner = "simonrw";
    repo = "gh-rebase-pr";
    rev = "5e304a1801997db5272fc852fc7227b6f809f889";
    hash = "sha256-Q8MLicdNhzTJHw6nkLvZtPMT4xSvK63lwl6KG31xNwE=";
  };
  doBuild = false;
  installPhase = ''
    install -Dm755 ./gh-rebase-pr $out/bin/gh-rebase-pr
  '';
}
