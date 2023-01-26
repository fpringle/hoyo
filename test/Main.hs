module Main (main) where

import HoYo.Test.Bookmark

import Control.Monad
import System.Exit

main :: IO ()
main = do
  res <- bookmarkTests
  unless res exitFailure
