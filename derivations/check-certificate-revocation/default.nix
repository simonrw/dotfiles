{ 
  pkgs ? import <nixpkgs> {}
  , ...
}:
pkgs.writers.writePython3Bin "check-certificate-revocation" {
  libraries = with pkgs.python3Packages; [
    requests
    cryptography
  ];
  flakeIgnore = [
    "E501"
    "W503"
  ];
} (builtins.readFile ./check.py)
