{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-missing-signatures #-}
module HoYo.Types where

import Control.Monad.Except (ExceptT, runExceptT, MonadError)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
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

data Settings = Settings {
  }

newtype HoYoMonad a = HoYoMonad {
  unHoYo :: ExceptT String (ReaderT Config IO) a
  } deriving (Functor, Applicative, Monad, MonadError String, MonadReader Config, MonadIO)

runHoYo :: HoYoMonad a -> Config -> IO (Either String a)
runHoYo = runReaderT . runExceptT . unHoYo

makeLenses ''Config
makeLenses ''Bookmark
