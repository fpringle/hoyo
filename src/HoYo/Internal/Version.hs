{-|
Module      : HoYo.Internal.Version
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

This module just exports a string containing the current HoYo version.
-}

{-# LANGUAGE TemplateHaskell #-}

module HoYo.Internal.Version (
  versionString
  ) where

import Data.Version.Package

-- | The current @hoyo@ version.
versionString :: String
versionString = $$(packageVersionStringTH "hoyo.cabal")
