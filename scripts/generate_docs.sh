#!/usr/bin/bash

index=$(cabal haddock --enable-documentation | tee /dev/stderr | tail -n1)
doc_dir=$(dirname $index)
rm -rf docs
mv $doc_dir docs
