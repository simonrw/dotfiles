{pkgs ? import <nixpkgs> {}}:
with pkgs;
  mkShell rec {
    packages = [
      rustup
      clang
      pkg-config
      mold
      dbus
    ];
    RUST_SRC_PATH = "${rustPlatform.rustLibSrc}";
    LD_LIBRARY_PATH = lib.makeLibraryPath packages;
  }
