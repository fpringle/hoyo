#!/usr/bin/bash

index=$(cabal new-haddock --enable-documentation --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' --haddock-hyperlink-source --haddock-quickjump | tee /dev/stderr | tail -n1)
doc_dir=$(dirname $index)
rm -rf docs
cp -r $doc_dir docs
