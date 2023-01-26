{-|
Module      : HoYo.Internal.Utils
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

Utility functions used by all the main HoYo.* modules.
-}

{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module HoYo.Internal.Utils where

import HoYo.Internal.Types

import Data.Bifunctor (bimap, first)
import Data.Char (isAscii, ord)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (stimes)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO

import Control.Applicative
import Text.Printf (printf)
import Text.Read (readEither)

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError(..), liftEither, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (MonadReader, asks)

import Lens.Micro
import Lens.Micro.Extras

import qualified Toml hiding (parse)
import qualified Toml.Parser.Core as Toml (eof, errorBundlePretty, parse)
import qualified Toml.Parser.Value as Toml

import Data.Time

import System.Directory

-- | A version of the lens "use" function for 'MonadReader'.
asks' :: MonadReader a m => SimpleGetter a b -> m b
asks' = asks . view

-- | Take the maximum of a list, with a default value if the list is empty.
maximumDefault :: Ord a => a -> [a] -> a
maximumDefault def [] = def
maximumDefault _ xs = maximum xs

-- | Throw an error if a check fails.
assert :: T.Text -> HoYoMonad Bool -> HoYoMonad ()
assert err check = do
  res <- check
  unless res $ throwError err

-- | Throw an error if a check fails AND the "fail_on_error" flag is set.
assertVerbose :: T.Text -> HoYoMonad Bool -> HoYoMonad Bool
assertVerbose err check = do
  shouldFail <- asks' (config . failOnError)
  res <- check
  when (shouldFail && not res) $ throwError err
  return res

-- | Get a list of the pairs in a Toml hashmap.
pairsToKeyVals :: HashMap.HashMap Toml.Key Toml.AnyValue -> [(Toml.Key, Toml.AnyValue)]
pairsToKeyVals = HashMap.toList

-- | Get a list of the pairs in a prefixmap of Toml tables.
tablesToKeyVals :: Toml.PrefixMap Toml.TOML -> [(Toml.Key, Toml.AnyValue)]
tablesToKeyVals = concatMap helper . Toml.toList
  where
    helper (k, toml) = [(k <> k2, v) | (k2, v) <- tomlToKeyVals toml]

-- | Get a list of the pairs in a prefixmap of Toml tables arrays.
tableArraysToKeyVals :: HashMap.HashMap Toml.Key (NE.NonEmpty Toml.TOML) -> [(Toml.Key, Toml.AnyValue)]
tableArraysToKeyVals = concatMap helper . HashMap.toList
  where
    helper :: (Toml.Key, NE.NonEmpty Toml.TOML) -> [(Toml.Key, Toml.AnyValue)]
    helper (k, ne) = [(k <> k2, v) | toml <- NE.toList ne, (k2, v) <- tomlToKeyVals toml]

-- | Convert a 'Toml.TOML' object to a list of keys and values.
tomlToKeyVals :: Toml.TOML -> [(Toml.Key, Toml.AnyValue)]
tomlToKeyVals toml =
  pairsToKeyVals (Toml.tomlPairs toml)
  <> tablesToKeyVals (Toml.tomlTables toml)
  -- <> tableArraysToKeyVals (Toml.tomlTableArrays toml) -- TODO

-- | Text representation of a Toml value. Copied directly from 'Toml.Type.Printer' source code.
valText :: Toml.Value t -> T.Text
valText (Toml.Bool b)    = T.toLower $ tshow b
valText (Toml.Integer n) = tshow n
valText (Toml.Double dub)  = showDouble dub
  where
    showDouble :: Double -> T.Text
    showDouble d | isInfinite d && d < 0 = "-inf"
                 | isInfinite d = "inf"
                 | isNaN d = "nan"
                 | otherwise = tshow d
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
valText (Toml.Local l)   = tshow l
valText (Toml.Day d)     = tshow d
valText (Toml.Hours h)   = tshow h
valText (Toml.Array arr)   = withLines Toml.defaultOptions valText arr
  where
    withLines :: Toml.PrintOptions -> (Toml.Value t -> T.Text) -> [Toml.Value t] -> T.Text
    withLines Toml.PrintOptions{..} valTxt a = case printOptionsLines of
        Toml.OneLine -> "[" <> T.intercalate ", " (map valTxt a) <> "]"
        Toml.MultiLine -> off <> "[ " <> T.intercalate (off <> ", ") (map valTxt a) <> off <> "]"
      where
        off :: T.Text
        off = "\n" <> stimes printOptionsIndent " "

-- | Given a file name and an extension, try to find a suitable path for
-- backing up that file. Used by 'backupFile'.
getBackupFile :: (MonadIO m, MonadError T.Text m) => TFilePath -> String -> m TFilePath
getBackupFile fp ext = do
  let fpStr = T.unpack fp
  ex <- liftIO $ doesFileExist fpStr
  unless ex $ throwError ("not a file: " <> T.pack fpStr)
  let firstTry = fpStr <> "." <> ext
  firstExists <- liftIO $ doesFileExist firstTry
  if firstExists
  then T.pack <$> getBackupFile' fpStr 2
  else return $ T.pack firstTry

  where
    getBackupFile' :: (MonadIO m, MonadError T.Text m) => String -> Int -> m String
    getBackupFile' file' n = do
      let file = file' <> "." <> show n <> ext
      fileExists <- liftIO $ doesFileExist file
      if fileExists
      then getBackupFile' file' (n + 1)
      else return file

-- | Try to back-up a file. Used when the "backup_before_clear" option is set.
backupFile :: (MonadIO m, MonadError T.Text m) => TFilePath -> String -> m ()
backupFile fp ext = do
  file <- getBackupFile fp ext
  liftIO $ copyFileWithMetadata (T.unpack fp) (T.unpack file)

-- | Try to read a 'Bool'.
readBool :: MonadError T.Text m => T.Text -> m Bool
readBool s = liftEither $ first T.pack (
              readEither sStr
                <|> first Toml.errorBundlePretty (Toml.parse (Toml.boolP <* Toml.eof) "" s)
                <|> Left ("Couldn't parse bool: " <> sStr)
              )
  where sStr = T.unpack s

-- | Try to read an 'Int'.
readInt :: MonadError T.Text m => T.Text -> m Int
readInt s = liftEither $ first T.pack (
              readEither sStr
                <|> bimap Toml.errorBundlePretty fromIntegral (Toml.parse (Toml.integerP <* Toml.eof) "" s)
                <|> Left ("Couldn't parse integer: " <> sStr)
              )
  where sStr = T.unpack s

-- | Print to stderr.
printStderr :: MonadIO m => T.Text -> m ()
printStderr = liftIO . T.hPutStrLn stderr

-- | Print to stdout.
printStdout :: MonadIO m => T.Text -> m ()
printStdout = liftIO . T.putStrLn

-- | Format a 'Bookmark'. Used for the "list" command and error reporting
-- during other commands.
--
-- @formatBookmark displayTime numberWidth bm@ returns a pretty representation
-- of @bm@, optionally showing the creation time, and padding the index to a
-- certain width.
formatBookmark :: Bool -> Int -> Bookmark -> T.Text
formatBookmark shouldDisplayTime indexWidth (Bookmark dir idx zTime mbName) =
  let num = T.justifyRight indexWidth ' ' $ tshow idx
      timeStr = T.pack $ formatTime defaultTimeLocale "%D %T" zTime
      d = case mbName of Nothing    -> dir
                         Just name  -> dir <> " " <> name

  in if shouldDisplayTime
     then num <> ". " <> timeStr <> "\t" <> d
     else num <> ". " <> d

-- | Format a list of 'Bookmark's. Used for the "list" command and error reporting
-- during other commands
--
-- @formatBookmark displayTime bms@ returns a pretty representation
-- of @bms@, optionally showing the creation time, and padding the indices to
-- line up.
formatBookmarks :: Bool -> [Bookmark] -> [T.Text]
formatBookmarks shouldDisplayTime bms = map (formatBookmark shouldDisplayTime indexWidth) bms
  where
    indexWidth = maximumDefault 1 $ map (length . show . view bookmarkIndex) bms

-- | Show a value as a 'T.Text' instead of a 'String'.
tshow :: Show a => a -> T.Text
tshow = T.pack . show
