{
  rustPlatform,
  pkg-config,
  dbus,
  ...
}:
rustPlatform.buildRustPackage rec {
  pname = "wlman";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [
    pkg-config
  ];

  propagatedBuildInputs = [
    dbus
  ];

  cargoLock = {
    lockFile = ./Cargo.lock;
  };

  meta = {
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
