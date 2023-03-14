module Main (main) where

import Control.Monad

import Hoyo.Test.Bookmark
import Hoyo.Test.CLI.Parse
import Hoyo.Test.Env
import Hoyo.Test.Utils

import System.Exit

main :: IO ()
main = do
  res <- and <$> sequence [
                            bookmarkTests
                          , utilsTests
                          , envTests
                          , parseTests
                          ]
  unless res exitFailure
