module HoYo.Bookmark where

import HoYo.Types

import Data.List
import qualified Data.Text as T
import Data.Bifunctor (first)

import Control.Monad.IO.Class
import Control.Monad (void)

import Toml (TomlCodec)
import qualified Toml

import Lens.Simple

bookmarkCodec :: TomlCodec Bookmark
bookmarkCodec = Bookmark
  <$> Toml.string     "directory"           .== _bookmarkDirectory
  <*> Toml.int        "index"               .== _bookmarkIndex
  <*> Toml.zonedTime  "created"             .== _bookmarkCreationTime
  <*> Toml.dioptional (Toml.string "name")  .== _bookmarkName

bookmarksCodec :: TomlCodec Bookmarks
bookmarksCodec = Toml.diwrap (Toml.list bookmarkCodec "bookmarks")

defaultBookmarks :: Bookmarks
defaultBookmarks = Bookmarks []

decodeBookmarks :: T.Text -> Either T.Text Bookmarks
decodeBookmarks = first Toml.prettyTomlDecodeErrors . Toml.decodeExact bookmarksCodec

decodeBookmarksFile :: MonadIO m => FilePath -> m (Either T.Text Bookmarks)
decodeBookmarksFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact bookmarksCodec

encodeBookmarks :: Bookmarks -> T.Text
encodeBookmarks = Toml.encode bookmarksCodec

encodeBookmarksFile :: MonadIO m => FilePath -> Bookmarks -> m ()
encodeBookmarksFile fp = void . Toml.encodeToFile bookmarksCodec fp

lookupBookmark :: Int -> Bookmarks -> Maybe Bookmark
lookupBookmark idx (Bookmarks bms) = find ((== idx) . view bookmarkIndex) bms

data BookmarkSearchTerm =
  SearchIndex Int
  | SearchName String

searchBookmarks :: BookmarkSearchTerm -> Bookmarks -> [Bookmark]
searchBookmarks (SearchIndex idx) (Bookmarks bms) = filter ((== idx) . view bookmarkIndex) bms
searchBookmarks (SearchName name) (Bookmarks bms) = filter ((== Just name) . view bookmarkName) bms
