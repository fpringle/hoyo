-- | The 'Bookmark' type provides a representation of bookmarks saved and used
-- by the hoyo program. This module exports some utility datatypes and functions
-- used for working with bookmarks.
module HoYo.Bookmark (
  -- * The Bookmark type
  Bookmark (..)
  , Bookmarks (..)
  , BookmarkSearchTerm (..)
  , formatBookmark
  , formatBookmarks
  , DefaultBookmark (..)

  -- *  Working with bookmarks
  , searchBookmarks
  , filterBookmarks
  , filterBookmarkByName
  , filterBookmarkByDirInfix
  , bookmarksFromDefault

  -- ** Parsing\/encoding bookmarks from\/to TOML
  , bookmarkCodec
  , bookmarksCodec
  , defaultBookmarkCodec
  , decodeBookmarks
  , decodeBookmarksFile
  , encodeBookmarks
  , encodeBookmarksFile
  ) where

import HoYo.Types
import HoYo.Utils

import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Function
import Data.List
import qualified Data.Text as T

import Control.Monad (forM, void)
import Control.Monad.IO.Class

import Data.Time

import qualified Toml
import Toml (TomlCodec)

import Lens.Micro.Extras

-- | A 'TomlCodec' for encoding and decoding 'Bookmark's.
bookmarkCodec :: TomlCodec Bookmark
bookmarkCodec = Bookmark
  <$> Toml.text       "directory"           .== _bookmarkDirectory
  <*> Toml.int        "index"               .== _bookmarkIndex
  <*> Toml.zonedTime  "created"             .== _bookmarkCreationTime
  <*> Toml.dioptional (Toml.text   "name")  .== _bookmarkName

-- | A 'TomlCodec' for encoding and decoding 'Bookmark's.
defaultBookmarkCodec :: TomlCodec DefaultBookmark
defaultBookmarkCodec = DefaultBookmark
  <$> Toml.text       "directory"           .== _defaultBookmarkDirectory
  <*> Toml.dioptional (Toml.text   "name")  .== _defaultBookmarkName

-- | A 'TomlCodec' for encoding and decoding 'Bookmarks'.
bookmarksCodec :: TomlCodec Bookmarks
bookmarksCodec = Toml.diwrap (Toml.list bookmarkCodec "bookmark")

-- | Decode a 'Bookmark' from a Text.
decodeBookmarks :: T.Text -> Either T.Text Bookmarks
decodeBookmarks = first Toml.prettyTomlDecodeErrors . Toml.decodeExact bookmarksCodec

-- | Decode a 'Bookmark' from a file.
decodeBookmarksFile :: MonadIO m => TFilePath -> m (Either T.Text Bookmarks)
decodeBookmarksFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact bookmarksCodec . T.unpack

-- | Encode a 'Bookmark' to a Text.
encodeBookmarks :: Bookmarks -> T.Text
encodeBookmarks = Toml.encode bookmarksCodec

-- | Encode a 'Bookmark' to a file.
encodeBookmarksFile :: MonadIO m => TFilePath -> Bookmarks -> m ()
encodeBookmarksFile fp = void . Toml.encodeToFile bookmarksCodec (T.unpack fp)

-- | @searchBookmarks searchTerm bookmarks@ partitions @bookmarks@ into a list of
-- 'Bookmark's that match the search term and a list of those that do not.
searchBookmarks :: BookmarkSearchTerm -> Bookmarks -> ([Bookmark], [Bookmark])
searchBookmarks (SearchIndex idx) (Bookmarks bms) =
  partition ((== idx) . view bookmarkIndex) bms
searchBookmarks (SearchName name) (Bookmarks bms) =
  partition (on (==) (fmap (T.map toLower)) (Just name) . view bookmarkName) bms

-- | A predicate used by 'filterBookmarks' - match on the bookmark name.
filterBookmarkByName :: Maybe T.Text -> Bookmark -> Bool
filterBookmarkByName Nothing = const True
filterBookmarkByName (Just name) = on (==) (fmap (T.map toLower)) (Just name) . view bookmarkName

-- | A predicate used by 'filterBookmarks' - match on the bookmark directory.
filterBookmarkByDirInfix :: Maybe T.Text -> Bookmark -> Bool
filterBookmarkByDirInfix Nothing = const True
filterBookmarkByDirInfix (Just pref) =
  on T.isInfixOf (T.dropWhileEnd (== '/')) pref . view bookmarkDirectory

combAnd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
combAnd pred1 pred2 a = on (&&) ($ a) pred1 pred2

-- | Given a bookmark name and a bookmark directory, test if a bookmark matches those
-- filters.
filterBookmarks :: Maybe T.Text -> Maybe T.Text -> Bookmark -> Bool
filterBookmarks name dirInfix = combAnd
                                    (filterBookmarkByName name)
                                    (filterBookmarkByDirInfix dirInfix)

-- | Convert a list of 'DefaultBookmark's to a 'Bookmarks', assiging indices and/
-- creation times on the fly.
bookmarksFromDefault :: MonadIO m => [DefaultBookmark] -> m Bookmarks
bookmarksFromDefault dbms = Bookmarks <$> bms
  where
    bms = forM (zip dbms [1..]) $ \(DefaultBookmark dir name, idx) -> do
      zTime <- liftIO getZonedTime
      return $ Bookmark dir idx zTime name
