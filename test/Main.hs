module Main (main) where

import HoYo

import Gen

import Test.QuickCheck

import Data.Function
import Control.Monad
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

testBookmarkSearch :: Property
testBookmarkSearch = forAll ((,) <$> genBookmarkSearchTerm <*> genBookmarks) testBookmarkSearch'

testBookmarkSearchSuccess :: Property
testBookmarkSearchSuccess = forAll ((,) <$> genBookmark <*> genBookmarks) testBookmarkSearchSuccess'

testBookmarkFilterByName' :: (Bookmark, Bookmark) -> Property
testBookmarkFilterByName' (bm1, bm2) = filterBookmarkByName name bm1
                                        .&&. (name === Nothing .||. not (filterBookmarkByName name bm2))

  where name = _bookmarkName bm1

testBookmarkFilterByName :: Property
testBookmarkFilterByName = forAll bookmarksWithDifferentNames testBookmarkFilterByName'
  where
    bookmarksWithDifferentNames = do
      bm1 <- genBookmark
      bm2 <- suchThat genBookmark (on (/=) _bookmarkName bm1)
      return (bm1, bm2)

tests :: [Property]
tests = [
  testBookmarkSearch
  , testBookmarkSearchSuccess
  , testBookmarkFilterByName
  ]

main :: IO ()
main = forM_ tests $ \prop -> do
  res <- quickCheckResult prop
  unless (isSuccess res) exitFailure
