{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use list comprehension" -}

module Hoyo.Test.CLI.Parse where

import qualified Data.Text           as T

import           Hoyo
import           Hoyo.CLI.Parse
import           Hoyo.Internal.Utils
import           Hoyo.Test.Gen       ()

import           Options.Applicative as O

import           Test.QuickCheck

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
