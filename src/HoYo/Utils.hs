{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module HoYo.Utils where

import HoYo.Types

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
import Control.Monad.Except (MonadError(..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (MonadReader(ask))
import Control.Monad.Trans.Reader (runReaderT)

import Lens.Micro
import Lens.Micro.Extras

import qualified Toml hiding (parse)
import qualified Toml.Parser.Core as Toml (errorBundlePretty, parse)
import qualified Toml.Parser.Value as Toml

import Data.Time

import System.Directory

-- | Given a hoyo 'Env', run a monadic action in IO.
runHoYo :: HoYoMonad a -> Env -> IO (Either T.Text a)
runHoYo = runReaderT . runExceptT . unHoYo

asks' :: MonadReader a m => SimpleGetter a b -> m b
asks' getter = view getter <$> ask

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

getBackupFile :: (MonadIO m, MonadError T.Text m) => FilePath -> String -> m FilePath
getBackupFile fp ext = do
  ex <- liftIO $ doesFileExist fp
  unless ex $ throwError ("not a file: " <> T.pack fp)
  let firstTry = fp <> "." <> ext
  firstExists <- liftIO $ doesFileExist firstTry
  if firstExists
  then getBackupFile' fp 2
  else return firstTry

  where
    getBackupFile' :: (MonadIO m, MonadError T.Text m) => FilePath -> Int -> m FilePath
    getBackupFile' file' n = do
      let file = file' <> "." <> show n <> ext
      fileExists <- liftIO $ doesFileExist file
      if fileExists
      then getBackupFile' file' (n + 1)
      else return file

-- | Try to back-up a file. Used when the "backup_before_clear" option is set.
backupFile :: (MonadIO m, MonadError T.Text m) => FilePath -> String -> m ()
backupFile fp ext = do
  file <- getBackupFile fp ext
  liftIO $ copyFileWithMetadata fp file

-- | Try to read a 'Bool'.
readBool :: MonadError T.Text m => T.Text -> m Bool
readBool s = liftEither $ first T.pack (
              readEither sStr
                <|> first Toml.errorBundlePretty (Toml.parse Toml.boolP "" s)
                <|> Left ("Couldn't parse bool: " <> sStr)
              )
  where sStr = T.unpack s

-- | Try to read an 'Int'.
readInt :: MonadError T.Text m => T.Text -> m Int
readInt s = liftEither $ first T.pack (
              readEither sStr
                <|> bimap Toml.errorBundlePretty fromIntegral (Toml.parse Toml.integerP "" s)
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
  let dText = T.pack dir
      num = T.justifyRight indexWidth ' ' $ tshow idx
      timeStr = T.pack $ formatTime defaultTimeLocale "%D %T" zTime
      d = case mbName of Nothing    -> dText
                         Just name  -> dText <> " " <> name

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

tshow :: Show a => a -> T.Text
tshow = T.pack . show
