{ rustPlatform
, fetchFromGitHub
}:
rustPlatform.buildRustPackage rec {
  pname = "cargo-dist";
  version = "0.0.2";

  src = fetchFromGitHub {
    owner = "axodotdev";
    repo = "cargo-dist";
    rev = "v${version}";
    hash = "sha256-7/TUk9LGwmHhKwFtwFQM7C/1ItRsoJ4IodeUPWfGjkc=";
  };

  cargoHash = "sha256-vmHPjecd1u0f8wSTu+LE2BNiZlskDADLXNjIj2v7D5E=";
}
