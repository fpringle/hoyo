{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    # library dependencies
    ansi-terminal base directory filepath microlens microlens-th mtl
    optparse-applicative package-version text time tomland transformers
    unordered-containers

    # executable dependencies
    base optparse-applicative text

    # test dependencies
    base directory filepath microlens optparse-applicative QuickCheck
    quickcheck-instances temporary text time
  ]);
in
pkgs.stdenv.mkDerivation rec {
  name = "hoyo";
  buildInputs = [ ghc ];
  shellHook =
    ''
    eval $(egrep ^export ${ghc}/bin/ghc)
    export PS1='\n\[\033[1;34m\][${name}@nix-shell:\w]\$\[\033[0m\] '
    '';
}
