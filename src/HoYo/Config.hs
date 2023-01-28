{-|
Module      : HoYo.Config
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Configuration for the hoyo program. This is stored on-disk as a TOML file,
usually at ~/.config/hoyo/config.toml
-}

{-# LANGUAGE RankNTypes #-}

module HoYo.Config (
  Config (..)

  , defaultConfig
  , decodeConfig
  , decodeConfigFile
  , encodeConfig
  , encodeConfigFile

  , setConfig
  , getKeyVals
  ) where

import HoYo.Internal.Config
import HoYo.Internal.Types
