{
  writers,
  python3Packages,
}:
writers.writePython3Bin "check-certificate-revocation" {
  libraries = with python3Packages; [
    requests
    cryptography
  ];
  flakeIgnore = [
    "E501"
    "W503"
  ];
} (builtins.readFile ./check.py)
