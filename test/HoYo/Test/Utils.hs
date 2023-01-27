{-# LANGUAGE TemplateHaskell #-}
module HoYo.Test.Utils where

import HoYo
import HoYo.Internal.Utils
import HoYo.Test.Gen
import HoYo.Test.HoYo

import qualified Data.Text as T
import Lens.Micro
import Test.QuickCheck

testMaximumDefaultInt :: Int -> [Int] -> Property
testMaximumDefaultInt x xs =
  if null xs
  then result === x
  else result === maximum xs
  where result = maximumDefault x xs

prop_MaximumDefault :: Property
prop_MaximumDefault = property testMaximumDefaultInt

testReadBool :: T.Text -> Either T.Text Bool -> Property
testReadBool tx expected = readBool tx === expected

prop_ReadBool :: Property
prop_ReadBool = forAll cases (uncurry testReadBool)
  where
    cases = oneof [elements correct, incorrect]

    correct = [(t, Right True) | t <- trues]
                <> [(f, Right False) | f <- falses]

    incorrect = do
      tx <- suchThat text (`notElem` (trues <> falses))
      return (tx, Left ("Couldn't parse bool: " <> tx))

    trues = ["true", "True"]
    falses = ["false", "False"]

testReadInt :: T.Text -> Either T.Text Int -> Property
testReadInt tx expected = readInt tx === expected

prop_ReadInt :: Property
prop_ReadInt = forAll cases (uncurry testReadInt)
  where
    cases = oneof [trues, falses]

    trues = do
      int <- arbitrary
      let tx = tshow int
      return (tx, Right int)

    falses = do
      lets1 <- listOf letter
      nondig <- elements (['a'..'z'] <> ['A'..'Z'])
      lets2 <- listOf letter
      let tx = T.pack (lets1 <> [nondig] <> lets2)
      return (tx, Left ("Couldn't parse integer: " <> tx))

prop_Assert :: T.Text -> Bool -> Property
prop_Assert errMsg True = testHoYoMonadEqWithEnv
                              (Bookmarks [])
                              defaultConfig
                              (assert errMsg (return True))
                              (Right ())
prop_Assert errMsg False = testHoYoMonadEqWithEnv
                              (Bookmarks [])
                              defaultConfig
                              (assert errMsg (return False))
                              (Left errMsg)

prop_AssertVerbose :: T.Text -> Bool -> Bool -> Property
prop_AssertVerbose errMsg bool verb = testHoYoMonadEqWithEnv
                                        (Bookmarks [])
                                        (set failOnError verb defaultConfig)
                                        (assertVerbose errMsg (return bool))
                                        (if not bool && verb then Left errMsg else Right bool)

return []
utilsTests :: IO Bool
utilsTests = $quickCheckAll
-- utilsTests = $verboseCheckAll
