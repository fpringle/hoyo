module HoYo.Bookmark where

import HoYo.Types

import Lens.Simple

import Toml (TomlCodec)
import qualified Toml

bookmarkCodec :: TomlCodec Bookmark
bookmarkCodec = Bookmark
  <$> Toml.string "directory"     .== _bookmarkDirectory
  <*> Toml.int    "index"         .== _bookmarkIndex

insertBookmark :: Bookmark -> Config -> Config
insertBookmark bm = over bookmarks (bm :)
