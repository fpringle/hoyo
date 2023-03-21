{-|
Module      : Hoyo.Bookmark
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

The 'Bookmark' type provides a representation of bookmarks saved and used
by the hoyo program. This module exports some utility datatypes and functions
used for working with bookmarks.
-}

module Hoyo.Bookmark (
  -- * The Bookmark type
  Bookmark (..)
  , Bookmarks (..)
  , BookmarkSearchTerm (..)
  , formatBookmark
  , formatBookmarks
  , DefaultBookmark (..)

  -- *  Working with bookmarks
  , getBookmarks
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

import           Control.Monad          (forM, void)
import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Data.Bifunctor         (first)
import           Data.Function
import           Data.List
import qualified Data.Text              as T
import           Data.Time

import           Hoyo.Internal.Types
import           Hoyo.Utils

import           Lens.Micro.Extras

import qualified Toml
import           Toml                   (TomlCodec, (.=))

-- | A 'TomlCodec' for encoding and decoding 'Bookmark's.
bookmarkCodec :: TomlCodec Bookmark
bookmarkCodec = Bookmark
  <$> Toml.string     "directory"           .= _bookmarkDirectory
  <*> Toml.int        "index"               .= _bookmarkIndex
  <*> Toml.zonedTime  "created"             .= _bookmarkCreationTime
  <*> Toml.dioptional (Toml.text   "name")  .= _bookmarkName

-- | A 'TomlCodec' for encoding and decoding 'Bookmark's.
defaultBookmarkCodec :: TomlCodec DefaultBookmark
defaultBookmarkCodec = DefaultBookmark
  <$> Toml.string     "directory"           .= _defaultBookmarkDirectory
  <*> Toml.dioptional (Toml.text   "name")  .= _defaultBookmarkName

-- | A 'TomlCodec' for encoding and decoding 'Bookmarks'.
bookmarksCodec :: TomlCodec Bookmarks
bookmarksCodec = Toml.diwrap (Toml.list bookmarkCodec "bookmark")

-- | Decode a 'Bookmark' from a Text.
decodeBookmarks :: T.Text -> Either HoyoException Bookmarks
decodeBookmarks = first (ParseException . pure . Toml.prettyTomlDecodeErrors)
                    . Toml.decodeExact bookmarksCodec

-- | Decode a 'Bookmark' from a file.
-- TODO: catch IO exceptions
decodeBookmarksFile :: (MonadIO m, MonadCatch m) => FilePath -> m (Either HoyoException Bookmarks)
decodeBookmarksFile = handle catchIOException
                        . fmap (first (ParseException . pure . Toml.prettyTomlDecodeErrors))
                        . Toml.decodeFileExact bookmarksCodec

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
  partition (on (==) (fmap T.toLower) (Just name) . view bookmarkName) bms

-- | A predicate used by 'filterBookmarks' - match on the bookmark name.
-- Note that matching is case-sensitive.
filterBookmarkByName :: Maybe T.Text -> Bookmark -> Bool
filterBookmarkByName Nothing = const True
filterBookmarkByName (Just name) = on (==) (fmap T.toLower) (Just name) . view bookmarkName

-- | A predicate used by 'filterBookmarks' - match on the bookmark directory.
filterBookmarkByDirInfix :: Maybe T.Text -> Bookmark -> Bool
filterBookmarkByDirInfix Nothing = const True
filterBookmarkByDirInfix (Just pref) =
  on T.isInfixOf (T.dropWhileEnd (== '/')) pref . T.pack . view bookmarkDirectory

-- | Given a bookmark name and a bookmark directory, test if a bookmark matches those
-- filters.
filterBookmarks :: Maybe T.Text -> Maybe T.Text -> Bookmark -> Bool
filterBookmarks name dirInfix bm = filterBookmarkByName name bm
                                     && filterBookmarkByDirInfix dirInfix bm

-- | Convert a list of 'DefaultBookmark's to a 'Bookmarks', assiging indices and/
-- creation times on the fly.
bookmarksFromDefault :: MonadIO m => [DefaultBookmark] -> m Bookmarks
bookmarksFromDefault dbms = Bookmarks <$> bms
  where
    bms = forM (zip dbms [1..]) $ \(DefaultBookmark dir name, idx) -> do
      zTime <- liftIO getZonedTime
      return $ Bookmark dir idx zTime name

-- | Get the bookmarks from the currently used bookmark file.
getBookmarks :: HoyoMonad Bookmarks
getBookmarks = asks' bookmarks
