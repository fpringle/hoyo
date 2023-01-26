{-# OPTIONS_GHC -fno-warn-orphans #-}
module HoYo.Test.Gen where

import HoYo

import Test.QuickCheck

import qualified Data.Text as T
import Data.Time
import System.FilePath

letter :: Gen Char
letter = elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])

text :: Gen T.Text
text = T.pack <$> listOf1 letter

string :: Gen String
string = listOf1 letter

genFilePath :: Gen TFilePath
genFilePath = T.pack . joinPath . ("/" :) <$> listOf string

genZonedTime :: Gen ZonedTime
genZonedTime = ZonedTime <$> genLocalTime <*> genTimeZone
  where
    genLocalTime = LocalTime <$> (ModifiedJulianDay <$> arbitrary)
                             <*> (TimeOfDay <$> chooseInt (0, 23)
                                            <*> chooseInt (0, 59)
                                            <*> arbitrary)

    genTimeZone = TimeZone <$> ((* 60) <$> chooseInt (-12, 12))
                           <*> arbitrary
                           <*> vectorOf 3 letter

instance Arbitrary Bookmark where
  arbitrary = Bookmark <$> genFilePath
                       <*> chooseInt (1, 1000)
                       <*> genZonedTime
                       <*> (Just <$> text)

instance Arbitrary Bookmarks where
  arbitrary = Bookmarks <$> listOf arbitrary

instance Arbitrary BookmarkSearchTerm where
  arbitrary = oneof [
    SearchIndex <$> chooseInt (1, 1000)
    , SearchName <$> text
    ]
