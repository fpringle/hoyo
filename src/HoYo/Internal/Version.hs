{-# LANGUAGE TemplateHaskell #-}
-- | This module just exports a string containing the current HoYo version.
module HoYo.Internal.Version (
  versionString
  ) where

import Data.Version.Package

-- | The current @hoyo@ version.
versionString :: String
versionString = $$(packageVersionStringTH "hoyo.cabal")
