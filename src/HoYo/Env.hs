{-|
Module      : HoYo.Env
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

The read-only hoyo environment.
-}

module HoYo.Env (
  -- * HoYo config
  Env (..)
  , initEnv
  , getEnv
  , writeEnv
  , readEnv

  -- ** Default file paths
  , defaultBookmarksPath
  , defaultConfigPath
  ) where

import HoYo.Internal.Env
import HoYo.Internal.Types
