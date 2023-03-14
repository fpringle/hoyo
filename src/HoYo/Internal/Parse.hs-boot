{-|
Module      : HoYo.Internal.Parse
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Parse CLI arguments.
-}

module HoYo.Internal.Parse where

import HoYo.Internal.Types

import qualified Data.Text as T
import Options.Applicative

parseCommand :: Parser Command
splitArgs :: T.Text -> [String]
options :: ParserInfo Options
