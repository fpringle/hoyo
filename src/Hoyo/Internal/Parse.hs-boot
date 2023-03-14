{-|
Module      : Hoyo.Internal.Parse
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Parse CLI arguments.
-}

module Hoyo.Internal.Parse where

import Hoyo.Internal.Types

import qualified Data.Text as T
import Options.Applicative

parseCommand :: Parser Command
splitArgs :: T.Text -> [String]
showHelp :: Maybe String -> IO ()
