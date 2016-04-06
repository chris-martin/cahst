{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_9" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./cahst.nix { }
