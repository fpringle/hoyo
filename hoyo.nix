{ mkDerivation, ansi-terminal, base, directory, filepath, lib
, microlens, microlens-th, mtl, optparse-applicative
, package-version, QuickCheck, quickcheck-instances, temporary
, text, time, tomland, transformers, unordered-containers
}:
mkDerivation {
  pname = "hoyo";
  version = "0.4.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base directory filepath microlens microlens-th mtl
    optparse-applicative package-version text time tomland transformers
    unordered-containers
  ];
  executableHaskellDepends = [
    ansi-terminal base directory filepath microlens microlens-th mtl
    optparse-applicative package-version text time tomland transformers
    unordered-containers
  ];
  testHaskellDepends = [
    ansi-terminal base directory filepath microlens microlens-th mtl
    optparse-applicative package-version QuickCheck
    quickcheck-instances temporary text time tomland transformers
    unordered-containers
  ];
  homepage = "https://github.com/fpringle/hoyo";
  description = "Bookmark directories for cd";
  license = lib.licenses.bsd3;
  mainProgram = "hoyo-cli";
}
