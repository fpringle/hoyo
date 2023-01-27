module Complete where

import HoYo

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import Options.Applicative

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

configKeyCompleter :: Completer
configKeyCompleter = listCompleter [
  "fail_on_error"
  , "display_creation_time"
  , "enable_clearing"
  , "enable_reset"
  , "backup_before_clear"
  , "default_command"
  ]

-- TODO: more sophisticated, needs to consider the current key
configValueCompleter :: Completer
configValueCompleter = listCompleter [
  "true"
  , "True"
  , "false"
  , "False"
  ]
