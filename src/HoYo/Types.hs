{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-missing-signatures #-}
module HoYo.Types where

import Lens.Simple

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

makeLenses ''Config
makeLenses ''Bookmark
