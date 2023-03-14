{-|
Module      : HoYo
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

hoyo is a command-line utility that lets the user save directories
as bookmarks (similar to in the browser) and easily @cd@ to them.
-}

module HoYo (
  -- * Bookmarks
  Bookmark (..)
  , Bookmarks (..)
  , searchBookmarks
  , filterBookmarks
  , module HoYo.Bookmark

  -- * Config
  , Config (..)
  , defaultConfig
  , setConfig
  , module HoYo.Config
  , module HoYo.Env

  -- * CLI commands
  , Command
  , runCommand
  , module HoYo.Command

  -- * Utility functions
  , runHoYo
  , withFiles
  , getEnvAndRunHoYo
  , getEnvAndRunCommand
  , HoYoMonad
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

import HoYo.Bookmark
import HoYo.Command
import HoYo.Config
import HoYo.Env
import HoYo.Internal.Types
import HoYo.Internal.Utils
import HoYo.Internal.Version

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Text as T
import System.Exit

-- | Given a hoyo 'Env', run a monadic action in IO.
runHoYo :: HoYoMonad a -> Env -> IO (Either T.Text a)
runHoYo = runReaderT . runExceptT . unHoYo

failure :: T.Text -> IO a
failure err = do
  printStderr ("Error: " <> err)
  exitWith (ExitFailure 1)

-- | @withFiles globals bFp sFp hoyo@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), applies the global
-- options and overrides in @globals@, and runs @hoyo@, returning either
-- the result or an error message.
withFiles :: GlobalOptions -> FilePath -> FilePath -> HoYoMonad a -> IO (Either T.Text a)
withFiles globals bFp sFp hoyo =
  getEnv bFp sFp >>= \case
    Left err    -> failure err
    Right env   -> runHoYo hoyo $ overrideEnv (overrides globals) env

-- | @getEnvAndRunHoYo globals hoyo bFp sFp@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), applies the global
-- options and overrides in @globals@, and runs @hoyo@, either printing an error
-- message or discarding the result.
getEnvAndRunHoYo :: GlobalOptions -> HoYoMonad a -> FilePath -> FilePath -> IO a
getEnvAndRunHoYo globals hoyo bFp sFp = withFiles globals bFp sFp hoyo >>= \case
  Left err  -> failure err
  Right res -> return res

-- | @getEnvAndRunHoYo opts bFp sFp@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), and runs the command
-- specified by @opts@.
getEnvAndRunCommand :: Options -> FilePath -> FilePath -> IO ()
getEnvAndRunCommand (Options cmd globals) bFp sFp = case cmd of
  Check opts  -> runCheck opts bFp sFp
  otherCmd    -> getEnvAndRunHoYo globals (runCommand otherCmd) bFp sFp
