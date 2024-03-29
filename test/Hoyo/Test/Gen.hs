{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Hoyo.Test.Gen where

import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T

import           Hoyo
import           Hoyo.Internal.Types

import           System.FilePath

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

letter :: Gen Char
letter = elements (['a'..'z'] <> ['A'..'Z'])

letterOrDigit :: Gen Char
letterOrDigit = elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])

text :: Gen T.Text
text = T.pack <$> listOf1 letterOrDigit

textNotNumber :: Gen T.Text
textNotNumber = T.pack . mconcat <$> sequence [listOf letterOrDigit, (: []) <$> letter, listOf letterOrDigit]

maybeText :: Gen (Maybe T.Text)
maybeText = liftArbitrary text

string :: Gen String
string = listOf1 letterOrDigit

genFilePath :: Gen FilePath
genFilePath = joinPath . ("/" :) <$> listOf string

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
                           -- <*> vectorOf 3 letterOrDigit

genNE :: Gen a -> Gen (NE.NonEmpty a)
genNE gen = (NE.:|) <$> gen <*> listOf gen

instance Arbitrary SearchException where
  arbitrary = oneof [
      NothingFound <$> arbitrary
    , TooManyResults <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary CommandException where
  arbitrary = oneof [
      SearchException <$> arbitrary
    , InvalidArgumentException <$> arbitrary
    , pure LoopException
    ]
  shrink = genericShrink

instance Arbitrary FileSystemException where
  arbitrary = oneof [
      NoFileException <$> genFilePath
    , NoDirException <$> genFilePath
    ]
  shrink = genericShrink

instance Arbitrary HoyoException where
  -- need to stop nested 'MultipleException's
  arbitrary = oneof (multiple : normal)
    where
      normal = [
          ConfigException <$> arbitrary
        , CommandException <$> arbitrary
        -- , IOException <$> _
        , FileSystemException <$> arbitrary
        , ParseException <$> arbitrary
        ]
      multiple = MultipleExceptions <$> genNE (oneof normal)

  -- shrink = genericShrink
  shrink = subterms


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
    , SearchName <$> textNotNumber
    ]

instance Arbitrary (ConfigValue 'TBool) where
  arbitrary = BoolV <$> arbitrary

instance Arbitrary (ConfigValue 'TDefaultBookmark) where
  arbitrary = DefaultBookmarkV <$> arbitrary

instance Arbitrary (ConfigValue 'TCommand) where
  arbitrary = CommandV <$> arbitrary

instance Arbitrary (ConfigValue t) => Arbitrary (ConfigValue ('TMaybe t)) where
  arbitrary = MaybeV <$> arbitrary

instance Arbitrary (ConfigValue t) => Arbitrary (ConfigValue ('TList t)) where
  arbitrary = ListOfV <$> arbitrary

instance Arbitrary Config where
  arbitrary = Config <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary

instance Arbitrary Env where
  arbitrary = Env <$> arbitrary
                  <*> genFilePath
                  <*> arbitrary
                  <*> genFilePath

instance Arbitrary AddOptions where
  arbitrary = AddOptions <$> genFilePath <*> maybeText

instance Arbitrary MoveOptions where
  arbitrary = MoveOptions <$> arbitrary

instance Arbitrary ListOptions where
  arbitrary = ListOptions <$> maybeText <*> maybeText <*> arbitrary

instance Arbitrary ClearOptions where
  arbitrary = pure ClearOptions

instance Arbitrary DeleteOptions where
  arbitrary = DeleteOptions <$> arbitrary

instance Arbitrary RefreshOptions where
  arbitrary = pure RefreshOptions

instance Arbitrary ConfigPrintOptions where
  arbitrary = ConfigPrintOptions <$> arbitrary

instance Arbitrary ConfigResetOptions where
  arbitrary = pure ConfigResetOptions

instance Arbitrary ConfigSetOptions where
  arbitrary = ConfigSetOptions <$> text <*> text

instance Arbitrary ConfigAddDefaultOptions where
  arbitrary = ConfigAddDefaultOptions <$> genFilePath <*> maybeText

instance Arbitrary ConfigCommand where
  arbitrary = oneof [ Print <$> arbitrary
                    , Reset <$> arbitrary
                    , Set <$> arbitrary
                    , AddDefaultBookmark <$> arbitrary
                    ]

instance Arbitrary CheckOptions where
  arbitrary = uncurry CheckOptions <$> suchThat arbitrary (uncurry (||))

instance Arbitrary Command where
  arbitrary = oneof [ Add <$> arbitrary
                    , Move <$> arbitrary
                    , List <$> arbitrary
                    , Clear <$> arbitrary
                    , Delete <$> arbitrary
                    , Refresh <$> arbitrary
                    , ConfigCmd <$> arbitrary
                    , Check <$> arbitrary
                    , pure DefaultCommand
                    ]

instance Arbitrary MaybeOverride where
  arbitrary = elements [OverrideFalse, OverrideTrue, NoOverride, Conflict]

instance Arbitrary OverrideOptions where
  arbitrary = OverrideOptions <$> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

instance Arbitrary GlobalOptions where
  arbitrary = GlobalOptions <$> liftArbitrary genFilePath
                            <*> liftArbitrary genFilePath
                            <*> arbitrary

instance Arbitrary Options where
  arbitrary = Options <$> arbitrary
                      <*> arbitrary
