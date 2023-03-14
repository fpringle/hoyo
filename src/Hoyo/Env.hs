{-|
Module      : Hoyo.Env
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

The read-only hoyo environment.
-}

module Hoyo.Env (
  -- * Hoyo config
  Env (..)
  , initEnv
  , getEnv
  , writeEnv
  , readEnv

  -- ** Default file paths
  , defaultBookmarksPath
  , defaultConfigPath
  ) where

import Hoyo.Internal.Env
import Hoyo.Internal.Types
