{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import HoYo

import Gen

import Test.QuickCheck

import Control.Monad
import Data.Function
import qualified Data.Text as T
import System.Exit

testBookmarkSearch' :: (BookmarkSearchTerm, Bookmarks) -> Property
testBookmarkSearch' (search, bms) =
  let
    (sat, notsat) = searchBookmarks search bms
    satGood = conjoin $ map (matchBm search) sat
    notsatGood = conjoin $ map (not . matchBm search) notsat
  in satGood .&&. notsatGood

  where
    matchBm (SearchName s) = (== Just s) . _bookmarkName
    matchBm (SearchIndex i) = (== i) . _bookmarkIndex

testBookmarkSearchSuccess' :: (Bookmark, Bookmarks) -> Property
testBookmarkSearchSuccess' (one, Bookmarks rest) = testBookmarkSearch' (search, Bookmarks (one:rest))
  where
    search = case _bookmarkName one of
                Just name   -> SearchName name
                Nothing     -> SearchIndex (_bookmarkIndex one)

prop_BookmarkSearch :: Property
prop_BookmarkSearch = forAll ((,) <$> genBookmarkSearchTerm <*> genBookmarks) testBookmarkSearch'

prop_BookmarkSearchSuccess :: Property
prop_BookmarkSearchSuccess = forAll ((,) <$> genBookmark <*> genBookmarks) testBookmarkSearchSuccess'

testBookmarkFilterByName' :: (Bookmark, Bookmark) -> Property
testBookmarkFilterByName' (bm1, bm2) =
  filterBookmarkByName name bm1
    .&&. (name === Nothing .||. not (filterBookmarkByName name bm2))

  where name = _bookmarkName bm1

prop_BookmarkFilterByName :: Property
prop_BookmarkFilterByName = forAll bookmarksWithDifferentNames testBookmarkFilterByName'
  where
    bookmarksWithDifferentNames = do
      bm1 <- genBookmark
      bm2 <- suchThat genBookmark (on (/=) _bookmarkName bm1)
      return (bm1, bm2)

testBookmarkFilterByDirInfix' :: (Bookmark, Bookmark) -> Property
testBookmarkFilterByDirInfix' (bm1, bm2) =
  filterBookmarkByDirInfix (Just dir) bm1
    .&&. not (filterBookmarkByDirInfix (Just dir) bm2)

  where dir = _bookmarkDirectory bm1

prop_BookmarkFilterByDirInfix :: Property
prop_BookmarkFilterByDirInfix = forAll bookmarksWithDifferentDirectories testBookmarkFilterByDirInfix'
  where
    bookmarksWithDifferentDirectories = do
      bm1 <- suchThat genBookmark ((/= "/") . _bookmarkDirectory)
      bm2 <- suchThat genBookmark (not . on T.isInfixOf _bookmarkDirectory bm1)
      return (bm1, bm2)

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  res <- runTests
  unless res exitFailure
