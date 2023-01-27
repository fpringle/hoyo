{-# OPTIONS_GHC -fno-warn-orphans #-}

module HoYo.Test.Gen where

import HoYo

import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances ()
-- import Data.Time
import System.FilePath

letter :: Gen Char
letter = elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])

text :: Gen T.Text
text = T.pack <$> listOf1 letter

string :: Gen String
string = listOf1 letter

genFilePath :: Gen TFilePath
genFilePath = T.pack . joinPath . ("/" :) <$> listOf string

-- genZonedTime :: Gen ZonedTime
-- genZonedTime = ZonedTime <$> genLocalTime <*> genTimeZone
  -- where
    -- genLocalTime = LocalTime <$> (ModifiedJulianDay <$> arbitrary)
                             -- <*> (TimeOfDay <$> chooseInt (0, 23)
                                            -- <*> chooseInt (0, 59)
                                            -- <*> arbitrary)
--
    -- genTimeZone = TimeZone <$> ((* 60) <$> chooseInt (-12, 12))
                           -- <*> arbitrary
                           -- <*> vectorOf 3 letter

instance Arbitrary Bookmark where
  arbitrary = Bookmark <$> genFilePath
                       <*> chooseInt (1, 1000)
                       <*> arbitrary
                       <*> (Just <$> text)

instance Arbitrary DefaultBookmark where
  arbitrary = DefaultBookmark <$> genFilePath <*> (Just <$> text)

instance Arbitrary Bookmarks where
  arbitrary = Bookmarks <$> listOf arbitrary

instance Arbitrary BookmarkSearchTerm where
  arbitrary = oneof [
      SearchIndex <$> chooseInt (1, 1000)
    , SearchName <$> text
    ]

genCommandText :: Gen T.Text
genCommandText = pure ""

instance Arbitrary Config where
  arbitrary = Config <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> oneof [Just <$> genCommandText, pure Nothing]

instance Arbitrary Env where
  arbitrary = Env <$> arbitrary
                  <*> genFilePath
                  <*> arbitrary
                  <*> genFilePath

instance Arbitrary ExecResult where
  arbitrary = oneof [
      pure Done
    , pure ShowHelp
    , ReRun <$> genCommandText
    ]
