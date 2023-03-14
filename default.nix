{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107" }:
let
  ghc = nixpkgs.pkgs.haskell.packages.${compiler};
in ghc.callPackage ./hoyo.nix { }
