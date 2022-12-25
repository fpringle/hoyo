{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-missing-signatures #-}
module HoYo.Types where

import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class

import Lens.Simple

import Toml ((.=))
import qualified Toml

import Data.Time

import System.IO.Error

(.==) :: Toml.Codec field a -> (object -> field) -> Toml.Codec object a
(.==) = (Toml..=)

data Env = Env {
  _bookmarks        :: !Bookmarks
  , _bookmarksPath  :: !FilePath
  , _config       :: !Config
  , _configPath   :: !FilePath
  }

data Bookmark = Bookmark {
  _bookmarkDirectory        :: !FilePath
  , _bookmarkIndex          :: !Int
  , _bookmarkCreationTime   :: !ZonedTime
  , _bookmarkName           :: !(Maybe String)
  } deriving Show

newtype Bookmarks = Bookmarks { unBookmarks :: [Bookmark] }
  deriving Show

data BookmarkSearchTerm =
  SearchIndex Int
  | SearchName String

instance Show BookmarkSearchTerm where
  show (SearchIndex idx) = '#' : show idx
  show (SearchName name) = name

data Config = Config {
  _failOnError            :: !Bool
  , _displayCreationTime  :: !Bool
  , _enableClearing       :: !Bool
  , _enableReset          :: !Bool
  , _backupBeforeClear    :: !Bool
  }

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
