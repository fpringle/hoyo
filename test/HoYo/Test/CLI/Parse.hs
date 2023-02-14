{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use list comprehension" -}

module HoYo.Test.CLI.Parse where

import HoYo
import HoYo.CLI.Parse
import HoYo.Internal.Utils
import HoYo.Test.Gen ()

import qualified Data.Text as T
import Options.Applicative as O
import Test.QuickCheck

testParserResult :: Options -> ParserResult Options -> Property
testParserResult a (O.Success b) = counterexample (T.unpack $ formatArgs $ formatOptions b) $ a === b
testParserResult _ _ = property False

prop_OptionsRoundTrip :: Options -> Property
prop_OptionsRoundTrip opts =
  let args = formatOptions opts
      result = execParserPure defaultPrefs options (map T.unpack args)
  in counterexample (T.unpack $ formatArgs args) $ testParserResult opts result

return []
parseTests :: IO Bool
parseTests = $quickCheckAll
