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

import Data.Time

import System.IO.Error

(.==) :: Toml.Codec field a -> (object -> field) -> Toml.Codec object a
(.==) = (Toml..=)

-- | The main hoyo read-only environment. Contains the current saved bookmarks,
-- the current hoyo configuration, and the file locations for each.
data Env = Env {
  _bookmarks        :: !Bookmarks
  , _bookmarksPath  :: !FilePath
  , _config         :: !Config
  , _configPath     :: !FilePath
  }

-- | Bookmark a directory for easy @cd@. A bookmark remembers the directory,
-- the index, the creation time, and optionally a user-specified nickname
-- for the bookmark.
data Bookmark = Bookmark {
  _bookmarkDirectory        :: !FilePath
  , _bookmarkIndex          :: !Int
  , _bookmarkCreationTime   :: !ZonedTime
  , _bookmarkName           :: !(Maybe String)
  } deriving Show

-- | Default bookmarks to save at init. A default bookmark remembers the directory
-- and optionally a user-specified nickname for the bookmark.
data DefaultBookmark = DefaultBookmark {
  _defaultBookmarkDirectory        :: !FilePath
  , _defaultBookmarkName           :: !(Maybe String)
  } deriving Show

-- | Wrapper for @['Bookmark']@.
newtype Bookmarks = Bookmarks { unBookmarks :: [Bookmark] }
  deriving Show

-- | Data-type for represting a bookmark search. You can either search
-- by index or by name. Used by the @delete@ and @move@ commands.
data BookmarkSearchTerm =
  SearchIndex Int
  | SearchName String

instance Show BookmarkSearchTerm where
  show (SearchIndex idx) = '#' : show idx
  show (SearchName name) = name

-- | A representation of hoyo settings.
data Config = Config {
  _failOnError            :: !Bool
  , _displayCreationTime  :: !Bool
  , _enableClearing       :: !Bool
  , _enableReset          :: !Bool
  , _backupBeforeClear    :: !Bool
  , _defaultBookmarks     :: ![DefaultBookmark]
  }

-- | 'HoYoMonad' is the main monad stack for the hoyo program. It's essentially a wrapper
-- around @ExceptT String (ReaderT Env IO)@: in other words,
-- @HoYoMonad a@ is equivalent to @Env -> IO (Either String a)@
newtype HoYoMonad a = HoYoMonad {
  unHoYo :: ExceptT String (ReaderT Env IO) a
  } deriving (Functor, Applicative, Monad, MonadError String, MonadReader Env)

instance MonadIO HoYoMonad where
  liftIO m = HoYoMonad (liftIO $ tryIOError m) >>= \case
    Left err      -> throwError ("IO error: " <> show err)
    Right result  -> return result

makeLenses ''Bookmark
makeLenses ''Config
makeLenses ''Env
