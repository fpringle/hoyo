module Main (main) where

import HoYo

import Gen

import Test.QuickCheck

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

main :: IO ()
main = do
  quickCheck testBookmarkSearch
  quickCheck testBookmarkSearchSuccess
