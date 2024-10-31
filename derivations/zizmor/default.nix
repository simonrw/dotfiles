{ rustPlatform
, fetchFromGitHub
, darwin
}:
rustPlatform.buildRustPackage rec {
  pname = "zizmor";
  version = "0.1.4";

  src = fetchFromGitHub {
    owner = "woodruffw";
    repo = "zizmor";
    rev = "v${version}";
    hash = "sha256-S2B4GQAqx4t9AZf3QDUhzku68j0buZdW0cLhmOiRssk=";
  };

  buildInputs = [
  ] ++ (with darwin.apple_sdk.frameworks; [
      SystemConfiguration
  ]);

  cargoHash = "sha256-hoZXR+zYuK/r4/r3QwIhTmMTCs5M0lMACH4QPEq07ZU=";
}
