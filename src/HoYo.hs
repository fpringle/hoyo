-- | hoyo is a command-line utility that lets the user save directories
-- as bookmarks (similar to in the browser) and easily @cd@ to them.
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
  , TFilePath
  ) where

import HoYo.Bookmark
import HoYo.Command
import HoYo.Config
import HoYo.Env
import HoYo.Types
import HoYo.Utils

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Text as T
import System.Exit

-- | Given a hoyo 'Env', run a monadic action in IO.
runHoYo :: HoYoMonad a -> Env -> IO (Either T.Text a)
runHoYo = runReaderT . runExceptT . unHoYo

failure :: T.Text -> IO ()
failure err = do
  printStderr ("Error: " <> err)
  exitWith (ExitFailure 1)

-- | @getEnvAndRunHoYo globals hoyo bFp sFp@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), applies the global
-- options and overrides in @globals@, and runs @hoyo@.
getEnvAndRunHoYo :: GlobalOptions -> HoYoMonad () -> TFilePath -> TFilePath -> IO ()
getEnvAndRunHoYo globals hoyo bFp sFp =
  getEnv bFp sFp >>= \case
    Left err    -> failure err
    Right env   -> do
      let overridenEnv = overrideEnv (overrides globals) env
      runHoYo hoyo overridenEnv >>= \case
        Left err  -> failure err
        Right _   -> return ()

-- | @getEnvAndRunHoYo opts bFp sFp@ gets the environment saved in
-- the bookmark path (@bFp@) and the config path (@sFp@), and runs the command
-- specified by @opts@.
getEnvAndRunCommand :: Options -> TFilePath -> TFilePath -> IO ()
getEnvAndRunCommand (Options cmd globals) bFp sFp = case cmd of
  Add opts        -> getEnvAndRunHoYo globals (runAdd opts) bFp sFp
  Move opts       -> getEnvAndRunHoYo globals (runMove opts) bFp sFp
  List opts       -> getEnvAndRunHoYo globals (runList opts) bFp sFp
  Clear opts      -> getEnvAndRunHoYo globals (runClear opts) bFp sFp
  Delete opts     -> getEnvAndRunHoYo globals (runDelete opts) bFp sFp
  Refresh opts    -> getEnvAndRunHoYo globals (runRefresh opts) bFp sFp
  ConfigCmd opts  -> getEnvAndRunHoYo globals (runConfig opts) bFp sFp
  Check opts      -> runCheck opts bFp sFp
