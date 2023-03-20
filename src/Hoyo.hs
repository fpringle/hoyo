{-|
Module      : Hoyo
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

hoyo is a command-line utility that lets the user save directories
as bookmarks (similar to in the browser) and easily @cd@ to them.
-}

module Hoyo (
  -- * Bookmarks
  Bookmark (..)
  , Bookmarks (..)
  , searchBookmarks
  , filterBookmarks
  , module Hoyo.Bookmark

  -- * Config
  , Config (..)
  , defaultConfig
  , setConfig
  , module Hoyo.Config
  , module Hoyo.Env

  -- * CLI commands
  , Command
  , runCommand
  , module Hoyo.Command

  -- * Utility functions
  , runHoyo
  , withFiles
  , getEnvAndRunHoyo
  , getEnvAndRunCommand
  , HoyoException (..)
  , HoyoMonad
  , modifyBookmarks
  , modifyBookmarksM
  , printStderr
  , printStdout
  , readInt
  , readBool
  , backupFile
  , assert
  , assertVerbose

  -- * Misc
  , versionString
  ) where

import Control.Monad.Except       (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)

import           Hoyo.Bookmark
import           Hoyo.Command
import           Hoyo.Config
import           Hoyo.Env
import           Hoyo.Internal.Types
import           Hoyo.Internal.Version
import           Hoyo.Utils

import           System.Exit

-- | Given a hoyo 'Env', run a monadic action in IO.
runHoyo :: HoyoMonad a -> Env -> IO (Either HoyoException a)
runHoyo = runReaderT . runExceptT . unHoyo

failure :: HoyoException -> IO a
failure err = do
  printStderr ("Error: " <> formatException err)
  exitWith (ExitFailure 1)

-- | @withFiles globals bFp sFp hoyo@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), applies the global
-- options and overrides in @globals@, and runs @hoyo@, returning either
-- the result or an error message.
withFiles :: GlobalOptions -> FilePath -> FilePath -> HoyoMonad a -> IO (Either HoyoException a)
withFiles globals bFp sFp hoyo =
  getEnv bFp sFp >>= \case
    Left err  -> failure err
    Right env -> runHoyo hoyo $ overrideEnv (overrides globals) env

-- | @getEnvAndRunHoyo globals hoyo bFp sFp@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), applies the global
-- options and overrides in @globals@, and runs @hoyo@, either printing an error
-- message or discarding the result.
getEnvAndRunHoyo :: GlobalOptions -> HoyoMonad a -> FilePath -> FilePath -> IO a
getEnvAndRunHoyo globals hoyo bFp sFp = withFiles globals bFp sFp hoyo >>= \case
  Left err  -> failure err
  Right res -> return res

-- | @getEnvAndRunHoyo opts bFp sFp@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), and runs the command
-- specified by @opts@.
getEnvAndRunCommand :: Options -> FilePath -> FilePath -> IO ()
getEnvAndRunCommand (Options cmd globals) bFp sFp = case cmd of
  Check opts -> runCheck opts bFp sFp
  otherCmd   -> getEnvAndRunHoyo globals (runCommand otherCmd) bFp sFp
