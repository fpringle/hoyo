{-|
Module      : Hoyo.CLI.Complete
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Bash competion functions used by the CLI.
-}

module Hoyo.CLI.Complete where

import           Control.Monad.IO.Class

import           Data.Maybe
import qualified Data.Text              as T

import           Hoyo

import           Options.Applicative

-- | Fetch existing data file and use it to complete bookmark names.
bookmarkCompleter :: Completer
bookmarkCompleter = listIOCompleter $ do
  sFp <- defaultConfigPath
  bFp <- defaultBookmarksPath
  res <- withFiles defaultGlobalOptions bFp sFp getBookmarks
  case res of
    Left err              -> do liftIO $ print err
                                return []
    Right (Bookmarks bms) -> do
      let indices = map (show . _bookmarkIndex) bms
      let nicknames = mapMaybe (fmap T.unpack . _bookmarkName) bms
      return (nicknames <> indices)

-- | Complete configuration keys.
configKeyCompleter :: Completer
configKeyCompleter = listCompleter [
  "fail_on_error"
  , "display_creation_time"
  , "enable_clearing"
  , "enable_reset"
  , "backup_before_clear"
  , "default_command"
  ]

-- | Complete configuration values.
--
-- TODO: could be more sophisticated, considering the current key
configValueCompleter :: Completer
configValueCompleter = listCompleter [
  "true"
  , "True"
  , "false"
  , "False"
  ]
