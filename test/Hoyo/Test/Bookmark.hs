{-# LANGUAGE TemplateHaskell #-}
module Hoyo.Test.Bookmark where

import           Control.Monad

import           Data.Function
import qualified Data.Text               as T

import           Hoyo
import           Hoyo.Internal.Types
import           Hoyo.Test.Gen           ()
import           Hoyo.Test.Hoyo

import           Lens.Micro.Extras

import           Test.QuickCheck
import           Test.QuickCheck.Monadic as Q

testBookmarkSearch :: BookmarkSearchTerm -> Bookmarks -> Property
testBookmarkSearch search bms =
  let
    (sat, notsat) = searchBookmarks search bms
    satGood = conjoin $ map (matchBm search) sat
    notsatGood = conjoin $ map (not . matchBm search) notsat
  in satGood .&&. notsatGood

  where
    matchBm (SearchName s) = (== Just (T.toLower s)) . fmap T.toLower . _bookmarkName
    matchBm (SearchIndex i) = (== i) . _bookmarkIndex

testBookmarkSearchSuccess :: Bookmark -> Bookmarks -> Property
testBookmarkSearchSuccess one (Bookmarks rest) = testBookmarkSearch search (Bookmarks (one:rest))
  where
    search = case _bookmarkName one of
                Just name -> SearchName name
                Nothing   -> SearchIndex (_bookmarkIndex one)

prop_BookmarkSearch :: Property
prop_BookmarkSearch = property testBookmarkSearch

prop_BookmarkSearchSuccess :: Property
prop_BookmarkSearchSuccess = property testBookmarkSearchSuccess

testBookmarkFilterByName :: Bookmark -> Bookmark -> Property
testBookmarkFilterByName bm1 bm2 =
  filterBookmarkByName name bm1
    .&&. (name === Nothing .||. not (filterBookmarkByName name bm2))

  where name = _bookmarkName bm1

prop_BookmarkFilterByName :: Property
prop_BookmarkFilterByName = forAll bookmarksWithDifferentNames (uncurry testBookmarkFilterByName)
  where
    bookmarksWithDifferentNames = do
      bm1 <- arbitrary
      bm2 <- suchThat arbitrary (on (/=) (fmap T.toLower . _bookmarkName) bm1)
      return (bm1, bm2)

testBookmarkFilterByDirInfix :: Bookmark -> Bookmark -> Property
testBookmarkFilterByDirInfix bm1 bm2 =
  filterBookmarkByDirInfix (Just dir) bm1
    .&&. not (filterBookmarkByDirInfix (Just dir) bm2)

  where dir = T.pack $ _bookmarkDirectory bm1

prop_BookmarkFilterByDirInfix :: Property
prop_BookmarkFilterByDirInfix = forAll bookmarksWithDifferentDirectories (uncurry testBookmarkFilterByDirInfix)
  where
    bookmarksWithDifferentDirectories = do
      bm1 <- suchThat arbitrary ((/= "/") . _bookmarkDirectory)
      bm2 <- suchThat arbitrary (not . on T.isInfixOf (T.pack . _bookmarkDirectory) bm1)
      return (bm1, bm2)

testBookmarksFromDefault :: [DefaultBookmark] -> Property
testBookmarksFromDefault defs = monadicIO $ do
  Bookmarks bms <- run (bookmarksFromDefault defs)
  Q.assert (length defs == length bms)
  forM_ (zip defs bms) $ \(def, bm) -> do
    Q.assert (view defaultBookmarkDirectory def == view bookmarkDirectory bm)
    Q.assert (view defaultBookmarkName def == view bookmarkName bm)

prop_BookmarksFromDefault :: Property
prop_BookmarksFromDefault = property testBookmarksFromDefault

bookmarksEq :: Bookmarks -> Bookmarks -> Bool
bookmarksEq (Bookmarks bms1) (Bookmarks bms2) =
  (length bms1 == length bms2)
    && all bmEq (zip bms1 bms2)

  where
    bmEq (Bookmark d1 i1 _ n1, Bookmark d2 i2 _ n2) =
      (d1 == d2)
        && (i1 == i2)
        && (n1 == n2)

eitherBookmarksEq :: Either a Bookmarks -> Either a Bookmarks -> Bool
eitherBookmarksEq (Right bm1) (Right bm2) = bookmarksEq bm1 bm2
eitherBookmarksEq _ _                     = False

testGetBookmarks :: Bookmarks -> Property
testGetBookmarks bms =
  testHoyoMonadPropertyWithEnv bms defaultConfig getBookmarks (eitherBookmarksEq (Right bms))

prop_GetBookmarks :: Property
prop_GetBookmarks = withMaxSuccess 10 testGetBookmarks

return []
bookmarkTests :: IO Bool
bookmarkTests = $quickCheckAll
