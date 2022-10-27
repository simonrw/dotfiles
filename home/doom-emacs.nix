{ pkgs ? import <nixpkgs> { } }:
{
  enable = true;
  doomPrivateDir = ./doom/doom.d;
}
