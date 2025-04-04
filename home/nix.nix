{...}: {
  nix.gc = {
    automatic = true;
    frequency = "weekly";
  };
}
