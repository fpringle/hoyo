{-# LANGUAGE RankNTypes #-}
{-|
Module      : HoYo.Config
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

Configuration for the hoyo program. This is stored on-disk as a TOML file,
usually at ~/.config/hoyo/config.toml
-}
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

import HoYo.Config.Internal
import HoYo.Types
