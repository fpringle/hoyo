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

(.==) :: Toml.Codec field a -> (object -> field) -> Toml.Codec object a
(.==) = (Toml..=)

data Config = Config {
  _bookmarks    :: ![Bookmark]
  , _settings   :: !Settings
  }

data Bookmark = Bookmark {
  _bookmarkDirectory    :: !FilePath
  , _bookmarkIndex      :: !Int
  }

instance Eq Bookmark where
  (Bookmark _ b1) == (Bookmark _ b2)  = b1 == b2 

instance Ord Bookmark where
  compare (Bookmark _ b1) (Bookmark _ b2) = compare b1 b2

data Settings = Settings {
  }

newtype HoYoMonad a = HoYoMonad {
  unHoYo :: ExceptT String (ReaderT Config IO) a
  } deriving (Functor, Applicative, Monad, MonadError String, MonadReader Config, MonadIO)

makeLenses ''Config
makeLenses ''Bookmark
