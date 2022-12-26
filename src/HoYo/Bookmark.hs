module HoYo.Bookmark (
  bookmarksCodec
  , defaultBookmarks
  , decodeBookmarks
  , decodeBookmarksFile
  , encodeBookmarks
  , encodeBookmarksFile
  , searchBookmarks
  , filterBookmarks
  , BookmarkSearchTerm (..)
  ) where

import HoYo.Types

import Data.List
import qualified Data.Text as T
import Data.Bifunctor (first)
import Data.Function
import Data.Char (toLower)

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

searchBookmarks :: BookmarkSearchTerm -> Bookmarks -> ([Bookmark], [Bookmark])
searchBookmarks (SearchIndex idx) (Bookmarks bms) =
  partition ((== idx) . view bookmarkIndex) bms
searchBookmarks (SearchName name) (Bookmarks bms) =
  partition (on (==) (fmap (map toLower)) (Just name) . view bookmarkName) bms

filterBookmarkByName :: Maybe String -> Bookmark -> Bool
filterBookmarkByName Nothing = const True
filterBookmarkByName (Just name) = on (==) (fmap (map toLower)) (Just name) . view bookmarkName

filterBookmarkByDirInfix :: Maybe String -> Bookmark -> Bool
filterBookmarkByDirInfix Nothing = const True
filterBookmarkByDirInfix (Just pref) =
  on isInfixOf (dropWhileEnd (== '/')) pref . view bookmarkDirectory

combAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
combAnd pred1 pred2 a = on (&&) ($ a) pred1 pred2

filterBookmarks :: Maybe String -> Maybe String -> Bookmark -> Bool
filterBookmarks name dirInfix = combAnd
                                    (filterBookmarkByName name)
                                    (filterBookmarkByDirInfix dirInfix)
