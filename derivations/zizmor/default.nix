{
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
  darwin,
  stdenv,
  lib,
}: let
  packages = [];

  darwin-packages = with darwin.apple_sdk.frameworks; [
    SystemConfiguration
  ];

  linux-packages = [
    openssl
  ];

  all-packages = packages ++ (lib.optionals stdenv.isDarwin darwin-packages) ++ (lib.optionals stdenv.isLinux linux-packages);
in
  rustPlatform.buildRustPackage rec {
    pname = "zizmor";
    version = "0.1.4";

    src = fetchFromGitHub {
      owner = "woodruffw";
      repo = "zizmor";
      rev = "v${version}";
      hash = "sha256-S2B4GQAqx4t9AZf3QDUhzku68j0buZdW0cLhmOiRssk=";
    };

    nativeBuildInputs = [
      pkg-config
    ];
    buildInputs = all-packages;

    cargoHash = "sha256-hoZXR+zYuK/r4/r3QwIhTmMTCs5M0lMACH4QPEq07ZU=";
  }
