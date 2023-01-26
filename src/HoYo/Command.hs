{-|
Module      : HoYo.Command
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

This module defines data-types and runner functions for the hoyo
command-line program.
-}

module HoYo.Command (
  -- * Running CLI commands
  runCommand
  , modifyBookmarks
  , modifyBookmarksM

  -- ** Specific command runners
  , runAdd
  , runMove
  , runList
  , runClear
  , runDelete
  , runRefresh
  , runConfig
  , runCheck
  , runDefaultCommand

  -- * Types
  , Options (..)
  , Command (..)
  , AddOptions (..)
  , MoveOptions (..)
  , ListOptions (..)
  , ClearOptions (..)
  , DeleteOptions (..)
  , RefreshOptions (..)
  , ConfigPrintOptions (..)
  , ConfigResetOptions (..)
  , ConfigSetOptions (..)
  , ConfigAddDefaultOptions (..)
  , ConfigCommand (..)
  , CheckOptions (..)
  , GlobalOptions (..)
  , defaultGlobalOptions
  , OverrideOptions (..)
  , defaultOverrideOptions
  , overrideConfig
  , overrideEnv
  , verifyOverrides
  , combOverride
  , MaybeOverride (..)
  ) where

import HoYo.Internal.Command
