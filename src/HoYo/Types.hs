{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-missing-signatures #-}
module HoYo.Types where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO)

import Lens.Simple

import Toml ((.=))
import qualified Toml

import Data.Time

(.==) :: Toml.Codec field a -> (object -> field) -> Toml.Codec object a
(.==) = (Toml..=)

data Env = Env {
  _bookmarks        :: !Bookmarks
  , _bookmarksPath  :: !FilePath
  , _settings       :: !Settings
  , _settingsPath   :: !FilePath
  }

data Bookmark = Bookmark {
  _bookmarkDirectory        :: !FilePath
  , _bookmarkIndex          :: !Int
  , _bookmarkCreationTime   :: !ZonedTime
  }

newtype Bookmarks = Bookmarks { unBookmarks :: [Bookmark] }

data Settings = Settings {
  _failOnError            :: !Bool
  , _displayCreationTime  :: !Bool
  }

newtype HoYoMonad a = HoYoMonad {
  unHoYo :: ExceptT String (ReaderT Env IO) a
  } deriving (Functor, Applicative, Monad, MonadError String, MonadReader Env, MonadIO)

makeLenses ''Bookmark
makeLenses ''Settings
makeLenses ''Env
