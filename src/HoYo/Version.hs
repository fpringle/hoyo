{-# LANGUAGE TemplateHaskell #-}
module HoYo.Version (
  versionString
  ) where

import Data.Version.Package

-- | The current @hoyo@ version.
versionString :: String
versionString = $$(packageVersionStringTH "hoyo.cabal")
