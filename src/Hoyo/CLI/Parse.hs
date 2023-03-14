{-|
Module      : Hoyo.CLI.Parse
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Parse CLI arguments.
-}

module Hoyo.CLI.Parse (
  -- * Parsing CLI arguments and options
    options
  , parseOptions
  , parseCommand
  , globalOptions
  , overrideOptions

  -- * Parsing specific CLI commands
  , addCommand
  , moveCommand
  , listCommand
  , clearCommand
  , deleteCommand
  , checkCommand
  , defaultCommand

  -- ** Parsing sub-commands for hoyo config
  , configCommand
  , configPrintCommand
  , configResetCommand
  , configSetCommand
  , configAddDefaultCommand

  -- * Misc/Utility
  , splitArgs
  ) where

import Hoyo.Internal.Parse
