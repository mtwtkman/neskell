{ nixpkgs ? import <nixpkgs> { } }:
nixpkgs.pkgs.haskell.compiler.ghcHEAD.callPackage ./nix/neskell.nix { }
