{-|
Module      : Hoyo.Config
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Configuration for the hoyo program. This is stored on-disk as a TOML file,
usually at ~/.config/hoyo/config.toml
-}

{-# LANGUAGE RankNTypes #-}

module Hoyo.Config (
  Config (..)

  , defaultConfig
  , decodeConfig
  , decodeConfigFile
  , encodeConfig
  , encodeConfigFile

  , setConfig
  , getKeyVals
  ) where

import Hoyo.Internal.Config
import Hoyo.Internal.Types
