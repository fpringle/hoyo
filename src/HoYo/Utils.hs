{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module HoYo.Utils where

import HoYo.Types

import Data.List (foldl')
import Data.Char (ord, isAscii)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup (stimes)

import Text.Printf (printf)

import Control.Monad (when, unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader (ask))

import Lens.Simple

import qualified Toml

import Data.Time

runHoYo :: HoYoMonad a -> Env -> IO (Either String a)
runHoYo = runReaderT . runExceptT . unHoYo

asks' :: MonadReader a m => Getter a a' b b' -> m b
asks' getter = view getter <$> ask

maximumDefault :: Ord a => a -> [a] -> a
maximumDefault def [] = def
maximumDefault _ xs = maximum xs

assert :: String -> HoYoMonad Bool -> HoYoMonad ()
assert err check = do
  res <- check
  unless res $ throwError err

assertVerbose :: String -> HoYoMonad Bool -> HoYoMonad Bool
assertVerbose err check = do
  shouldFail <- asks' (settings . failOnError)
  res <- check
  when (shouldFail && not res) $ throwError err
  return res

pairsToKeyVals :: HashMap.HashMap Toml.Key Toml.AnyValue -> [(Toml.Key, Toml.AnyValue)]
pairsToKeyVals = HashMap.toList

tablesToKeyVals :: Toml.PrefixMap Toml.TOML -> [(Toml.Key, Toml.AnyValue)]
tablesToKeyVals = concatMap helper . Toml.toList
  where
    helper (k, toml) = [(k <> k2, v) | (k2, v) <- tomlToKeyVals toml]

tableArraysToKeyVals :: HashMap.HashMap Toml.Key (NE.NonEmpty Toml.TOML) -> [(Toml.Key, Toml.AnyValue)]
tableArraysToKeyVals = concatMap helper . HashMap.toList
  where
    helper :: (Toml.Key, NE.NonEmpty Toml.TOML) -> [(Toml.Key, Toml.AnyValue)]
    helper (k, ne) = [(k <> k2, v) | toml <- NE.toList ne, (k2, v) <- tomlToKeyVals toml]

tomlToKeyVals :: Toml.TOML -> [(Toml.Key, Toml.AnyValue)]
tomlToKeyVals toml =
  pairsToKeyVals (Toml.tomlPairs toml)
  <> tablesToKeyVals (Toml.tomlTables toml)
  -- <> tableArraysToKeyVals (Toml.tomlTableArrays toml) -- TODO

valText :: Toml.Value t -> T.Text
valText (Toml.Bool b)    = T.toLower $ showText b
valText (Toml.Integer n) = showText n
valText (Toml.Double dub)  = showDouble dub
  where
    showDouble :: Double -> T.Text
    showDouble d | isInfinite d && d < 0 = "-inf"
                 | isInfinite d = "inf"
                 | isNaN d = "nan"
                 | otherwise = showText d
valText (Toml.Text s)    = showTextUnicode s
  where
    showTextUnicode :: T.Text -> T.Text
    showTextUnicode text = T.pack $ show finalText
      where
        xss = T.unpack text
        finalText = foldl' (\acc (ch, asciiCh) -> acc ++ getCh ch asciiCh) "" asciiArr

        asciiArr = zip xss $ asciiStatus xss

        getCh :: Char -> Bool -> String
        getCh ch True  = [ch] -- it is true ascii character
        getCh ch False = printf "\\U%08x" (ord ch) :: String -- it is not true ascii character, it must be encoded

        asciiStatus :: String -> [Bool]
        asciiStatus = map isAscii
valText (Toml.Zoned zTime)   = showZonedTime zTime
  where
    showZonedTime :: ZonedTime -> T.Text
    showZonedTime t = T.pack $ showZonedDateTime t <> showZonedZone t
      where
        showZonedDateTime = formatTime defaultTimeLocale "%FT%T%Q"
        showZonedZone
            = (\(x,y) -> x ++ ":" ++ y)
            . (\z -> splitAt (length z - 2) z)
            . formatTime defaultTimeLocale "%z"
valText (Toml.Local l)   = showText l
valText (Toml.Day d)     = showText d
valText (Toml.Hours h)   = showText h
valText (Toml.Array arr)   = withLines Toml.defaultOptions valText arr
  where
    withLines :: Toml.PrintOptions -> (Toml.Value t -> T.Text) -> [Toml.Value t] -> T.Text
    withLines Toml.PrintOptions{..} valTxt a = case printOptionsLines of
        Toml.OneLine -> "[" <> T.intercalate ", " (map valTxt a) <> "]"
        Toml.MultiLine -> off <> "[ " <> T.intercalate (off <> ", ") (map valTxt a) <> off <> "]"
      where
        off :: T.Text
        off = "\n" <> stimes printOptionsIndent " "

showText :: Show a => a -> T.Text
showText = T.pack . show
