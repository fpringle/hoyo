{-|
Module      : HoYo.Bookmark
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

The 'Bookmark' type provides a representation of bookmarks saved and used
by the hoyo program. This module exports some utility datatypes and functions
used for working with bookmarks.
-}
module HoYo.Bookmark (
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

import HoYo.Internal.Bookmark
import HoYo.Types
import HoYo.Utils
