{-|
Module      : Hoyo.Internal.Version
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

This module just exports a string containing the current Hoyo version.
-}

{-# LANGUAGE TemplateHaskell #-}

module Hoyo.Internal.Version (
  versionString
  ) where

import Data.Version.Package

-- | The current @hoyo@ version.
versionString :: String
versionString = $$(packageVersionStringTH "hoyo.cabal")
