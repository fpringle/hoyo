module Main (main) where

import HoYo.Test.Bookmark
import HoYo.Test.Env
import HoYo.Test.Utils

import Control.Monad
import System.Exit

main :: IO ()
main = do
  res <- and <$> sequence [
                            bookmarkTests
                          , utilsTests
                          , envTests
                          ]
  unless res exitFailure
