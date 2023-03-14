module Main (main) where

import Control.Monad

import HoYo.Test.Bookmark
import HoYo.Test.CLI.Parse
import HoYo.Test.Env
import HoYo.Test.Utils

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
