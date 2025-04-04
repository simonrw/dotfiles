{pkgs, ...}:
with pkgs; let
  plugin = stdenv.mkDerivation (finalAttrs: {
    pname = "wireshark-dap";
    version = "unstable";

    src = fetchFromGitHub {
      owner = "glassechidna";
      repo = "wireshark-debug-adapter-protocol";
      rev = "0e315f22cc9a40f056bb676a9a7a068f2f9b6d42";
      hash = "sha256-UgxN3CdZ3XgwkYGtg33kJfNsORiYvK0eeVp8gOyK/R8=";
    };

    doConfigure = false;
    buildPhase = ":";
    installPhase = ''
      cp tcp-debug-adapter-protocol-dissector.lua $out
    '';
  });
in {
  home.file.".local/lib/wireshark/plugins/tcp-debug-adapter-protocol-dissector.lua" = {
    source = plugin;
  };
}
