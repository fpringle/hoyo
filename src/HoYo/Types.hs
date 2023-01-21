{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-missing-signatures #-}
module HoYo.Types where

import Control.Monad.Except (ExceptT, MonadError(..))
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)

import Lens.Micro.TH

import qualified Toml
import Toml ((.=))

import qualified Data.Text as T
import Data.Time

import System.IO.Error

(.==) :: Toml.Codec field a -> (object -> field) -> Toml.Codec object a
(.==) = (Toml..=)

-- | A 'T.Text' version of 'FilePath'.
type TFilePath = T.Text

-- | The main hoyo read-only environment. Contains the current saved bookmarks,
-- the current hoyo configuration, and the file locations for each.
data Env = Env {
  _bookmarks        :: !Bookmarks
  , _bookmarksPath  :: !TFilePath
  , _config         :: !Config
  , _configPath     :: !TFilePath
  }

-- | Bookmark a directory for easy @cd@. A bookmark remembers the directory,
-- the index, the creation time, and optionally a user-specified nickname
-- for the bookmark.
data Bookmark = Bookmark {
  _bookmarkDirectory        :: !TFilePath
  , _bookmarkIndex          :: !Int
  , _bookmarkCreationTime   :: !ZonedTime
  , _bookmarkName           :: !(Maybe T.Text)
  } deriving Show

-- | Default bookmarks to save at init. A default bookmark remembers the directory
-- and optionally a user-specified nickname for the bookmark.
data DefaultBookmark = DefaultBookmark {
  _defaultBookmarkDirectory        :: !TFilePath
  , _defaultBookmarkName           :: !(Maybe T.Text)
  } deriving Show

-- | Wrapper for @['Bookmark']@.
newtype Bookmarks = Bookmarks { unBookmarks :: [Bookmark] }
  deriving Show

-- | Data-type for represting a bookmark search. You can either search
-- by index or by name. Used by the @delete@ and @move@ commands.
data BookmarkSearchTerm =
  SearchIndex Int
  | SearchName T.Text

instance Show BookmarkSearchTerm where
  show (SearchIndex idx) = '#' : show idx
  show (SearchName name) = T.unpack name

-- | A representation of hoyo settings.
data Config = Config {
  _failOnError            :: !Bool
  , _displayCreationTime  :: !Bool
  , _enableClearing       :: !Bool
  , _enableReset          :: !Bool
  , _backupBeforeClear    :: !Bool
  , _defaultBookmarks     :: ![DefaultBookmark]
  , _defaultCommand       :: !(Maybe T.Text)
  }

-- | 'HoYoMonad' is the main monad stack for the hoyo program. It's essentially a wrapper
-- around @ExceptT T.Text (ReaderT Env IO)@: in other words,
-- @HoYoMonad a@ is equivalent to @Env -> IO (Either T.Text a)@
newtype HoYoMonad a = HoYoMonad {
  unHoYo :: ExceptT T.Text (ReaderT Env IO) a
  } deriving (Functor, Applicative, Monad, MonadError T.Text, MonadReader Env)

instance MonadIO HoYoMonad where
  liftIO m = HoYoMonad (liftIO $ tryIOError m) >>= \case
    Left err      -> throwError ("IO error: " <> T.pack (show err))
    Right result  -> return result

-- | The result of executing a command. Currently only used meaningfully
-- by 'HoYo.Command.runDefaultCommand'.
data ExecResult =
  Done
  | ShowHelp
  | ReRun T.Text

makeLenses ''Bookmark
makeLenses ''Config
makeLenses ''Env
