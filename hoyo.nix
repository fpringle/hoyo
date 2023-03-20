{ mkDerivation, ansi-terminal, base, directory, filepath, json
, lib, microlens, microlens-th, mtl, optparse-applicative
, package-version, pager, QuickCheck, quickcheck-instances
, template-haskell, temporary, text, time, tomland, transformers
, unordered-containers
}:
mkDerivation rec {
  pname = "hoyo";
  version = "0.5.1.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base directory filepath json microlens microlens-th
    mtl optparse-applicative package-version pager text time tomland
    template-haskell transformers unordered-containers
  ];
  executableHaskellDepends = libraryHaskellDepends;
  testHaskellDepends = libraryHaskellDepends ++ [
    QuickCheck quickcheck-instances temporary
  ];
  homepage = "https://github.com/fpringle/hoyo";
  description = "Bookmark directories for cd";
  license = lib.licenses.bsd3;
  mainProgram = "hoyo-cli";
}
