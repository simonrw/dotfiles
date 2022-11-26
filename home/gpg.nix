{ pkgs }:
{
  enable = true;
  mutableTrust = true;
  mutableKeys = true;
  settings = {
    keyserver = "keyserver.ubuntu.com";
  };
}
