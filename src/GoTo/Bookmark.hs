module GoTo.Bookmark where

import GoTo.Types

import Toml (TomlCodec, (.=))
import qualified Toml

bookmarkCodec :: TomlCodec Bookmark
bookmarkCodec = Bookmark
  <$> Toml.string "directory"     .= _bookmarkDirectory
  <*> Toml.int    "index"         .= _bookmarkIndex
