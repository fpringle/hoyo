{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module HoYo.Utils where

import HoYo.Types

import Data.List
import Data.Char (ord, isAscii)
import Data.Bifunctor (first, bimap)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup (stimes)

import System.IO

import Text.Printf (printf)
import Text.Read (readEither)
import Control.Applicative

import Control.Monad (when, unless)
import Control.Monad.Except (MonadError(..), runExceptT, throwError, liftEither)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.IO.Class

import Lens.Simple

import qualified Toml hiding (parse)
import qualified Toml.Parser.Value as Toml
import qualified Toml.Parser.Core as Toml (parse, errorBundlePretty)

import Data.Time

import System.Directory

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
  shouldFail <- asks' (config . failOnError)
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

getBackupFile :: (MonadIO m, MonadError String m) => FilePath -> String -> m FilePath
getBackupFile fp ext = do
  ex <- liftIO $ doesFileExist fp
  unless ex $ throwError ("not a file: " <> fp)
  let firstTry = fp <> "." <> ext
  firstExists <- liftIO $ doesFileExist firstTry
  if firstExists
  then getBackupFile' fp 2
  else return firstTry

  where
    getBackupFile' :: (MonadIO m, MonadError String m) => FilePath -> Int -> m FilePath
    getBackupFile' file' n = do
      let file = file' <> "." <> show n <> ext
      fileExists <- liftIO $ doesFileExist file
      if fileExists
      then getBackupFile' file' (n + 1)
      else return file

backupFile :: (MonadIO m, MonadError String m) => FilePath -> String -> m ()
backupFile fp ext = do
  file <- getBackupFile fp ext
  liftIO $ copyFileWithMetadata fp file

readBool :: MonadError String m => String -> m Bool
readBool s = liftEither (
              readEither s
                <|> first Toml.errorBundlePretty (Toml.parse Toml.boolP "" $ T.pack s)
                <|> Left ("Couldn't parse bool: " <> s)
              )

readInt :: MonadError String m => String -> m Int
readInt s = liftEither (
              readEither s
                <|> bimap Toml.errorBundlePretty fromIntegral (Toml.parse Toml.integerP "" $ T.pack s)
                <|> Left ("Couldn't parse integer: " <> s)
              )

printStderr :: MonadIO m => String -> m ()
printStderr = liftIO . hPutStrLn stderr

printStdout :: MonadIO m => String -> m ()
printStdout = liftIO . putStrLn
