-- | The 'Bookmark' type provides a representation of bookmarks saved and used
-- by the hoyo program. This module exports some utility datatypes and functions
-- used for working with bookmarks.
module HoYo.Bookmark (
  -- * The Bookmark type
  Bookmark (..)
  , Bookmarks (..)
  , BookmarkSearchTerm (..)

  -- *  Working with bookmarks
  , searchBookmarks
  , filterBookmarks
  , filterBookmarkByName
  , filterBookmarkByDirInfix

  -- ** Parsing\/encoding bookmarks from\/to TOML
  , bookmarkCodec
  , bookmarksCodec
  , decodeBookmarks
  , decodeBookmarksFile
  , encodeBookmarks
  , encodeBookmarksFile

  -- * Bookmark lenses
  -- , bookmarkDirectory
  -- , bookmarkIndex
  -- , bookmarkCreationTime
  -- , bookmarkName
  ) where

import HoYo.Types

import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Function
import Data.List
import qualified Data.Text as T

import Control.Monad (void)
import Control.Monad.IO.Class

import qualified Toml
import Toml (TomlCodec)

import Lens.Simple

-- | A 'TomlCodec' for encoding and decoding 'Bookmark's.
bookmarkCodec :: TomlCodec Bookmark
bookmarkCodec = Bookmark
  <$> Toml.string     "directory"           .== _bookmarkDirectory
  <*> Toml.int        "index"               .== _bookmarkIndex
  <*> Toml.zonedTime  "created"             .== _bookmarkCreationTime
  <*> Toml.dioptional (Toml.string "name")  .== _bookmarkName

-- | A 'TomlCodec' for encoding and decoding 'Bookmarks'.
bookmarksCodec :: TomlCodec Bookmarks
bookmarksCodec = Toml.diwrap (Toml.list bookmarkCodec "bookmarks")

-- | Decode a 'Bookmark' from a Text.
decodeBookmarks :: T.Text -> Either T.Text Bookmarks
decodeBookmarks = first Toml.prettyTomlDecodeErrors . Toml.decodeExact bookmarksCodec

-- | Decode a 'Bookmark' from a file.
decodeBookmarksFile :: MonadIO m => FilePath -> m (Either T.Text Bookmarks)
decodeBookmarksFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact bookmarksCodec

-- | Encode a 'Bookmark' to a Text.
encodeBookmarks :: Bookmarks -> T.Text
encodeBookmarks = Toml.encode bookmarksCodec

-- | Encode a 'Bookmark' to a file.
encodeBookmarksFile :: MonadIO m => FilePath -> Bookmarks -> m ()
encodeBookmarksFile fp = void . Toml.encodeToFile bookmarksCodec fp

-- | @searchBookmarks searchTerm bookmarks@ partitions @bookmarks@ into a list of
-- 'Bookmark's that match the search term and a list of those that do not.
searchBookmarks :: BookmarkSearchTerm -> Bookmarks -> ([Bookmark], [Bookmark])
searchBookmarks (SearchIndex idx) (Bookmarks bms) =
  partition ((== idx) . view bookmarkIndex) bms
searchBookmarks (SearchName name) (Bookmarks bms) =
  partition (on (==) (fmap (map toLower)) (Just name) . view bookmarkName) bms

-- | A predicate used by 'filterBookmarks' - match on the bookmark name.
filterBookmarkByName :: Maybe String -> Bookmark -> Bool
filterBookmarkByName Nothing = const True
filterBookmarkByName (Just name) = on (==) (fmap (map toLower)) (Just name) . view bookmarkName

-- | A predicate used by 'filterBookmarks' - match on the bookmark directory.
filterBookmarkByDirInfix :: Maybe String -> Bookmark -> Bool
filterBookmarkByDirInfix Nothing = const True
filterBookmarkByDirInfix (Just pref) =
  on isInfixOf (dropWhileEnd (== '/')) pref . view bookmarkDirectory

combAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
combAnd pred1 pred2 a = on (&&) ($ a) pred1 pred2

-- | Given a bookmark name and a bookmark directory, test if a bookmark matches those
-- filters.
filterBookmarks :: Maybe String -> Maybe String -> Bookmark -> Bool
filterBookmarks name dirInfix = combAnd
                                    (filterBookmarkByName name)
                                    (filterBookmarkByDirInfix dirInfix)
