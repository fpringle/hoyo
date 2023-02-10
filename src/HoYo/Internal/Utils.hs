{-|
Module      : HoYo.Internal.Utils
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Utility functions used by all the main HoYo.* modules.
-}

{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune #-}

module HoYo.Internal.Utils where

{- HLINT ignore "Use list comprehension" -}

import HoYo.Internal.Types

import Data.Bifunctor (bimap, first)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO

import Control.Applicative
import Text.Read (readEither)

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError(..), liftEither, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (MonadReader, asks)

import Lens.Micro
import Lens.Micro.Extras

import qualified Toml.Parser.Core as Toml (eof, errorBundlePretty, parse)
import qualified Toml.Parser.Value as Toml

import Data.Time

import System.Directory


-----------------------------------------
-- getters and setter for ConfigValue

getBool :: ConfigValue 'TBool -> Bool
getBool (BoolV bool) = bool

setBool :: ConfigValue 'TBool -> Bool -> ConfigValue 'TBool
setBool _ = BoolV

getDefaultBookmark :: ConfigValue 'TDefaultBookmark -> DefaultBookmark
getDefaultBookmark (DefaultBookmarkV bm) = bm

setDefaultBookmark :: ConfigValue 'TDefaultBookmark -> DefaultBookmark -> ConfigValue 'TDefaultBookmark
setDefaultBookmark _ = DefaultBookmarkV

getCommand :: ConfigValue 'TCommand -> Command
getCommand (CommandV t) = t

setCommand :: ConfigValue 'TCommand -> Command -> ConfigValue 'TCommand
setCommand _ = CommandV

getList :: ConfigValue ('TList t) -> [ConfigValue t]
getList (ListOfV xs) = xs

setList :: ConfigValue ('TList t) -> [ConfigValue t] -> ConfigValue ('TList t)
setList _ = ListOfV

getMaybe :: ConfigValue ('TMaybe t) -> Maybe (ConfigValue t)
getMaybe (MaybeV val) = val

setMaybe :: ConfigValue ('TMaybe t) -> Maybe (ConfigValue t) -> ConfigValue ('TMaybe t)
setMaybe _ = MaybeV

-----------------------------------------
-- Lenses for Config

failOnError :: Lens' Config Bool
failOnError = lens getter setter
  where
    getter = getBool . _failOnError
    setter cfg bool = cfg { _failOnError = setBool (_failOnError cfg) bool }

displayCreationTime :: Lens' Config Bool
displayCreationTime = lens getter setter
  where
    getter = getBool . _displayCreationTime
    setter cfg bool = cfg { _displayCreationTime = setBool (_displayCreationTime cfg) bool }

enableClearing :: Lens' Config Bool
enableClearing = lens getter setter
  where
    getter = getBool . _enableClearing
    setter cfg bool = cfg { _enableClearing = setBool (_enableClearing cfg) bool }

enableReset :: Lens' Config Bool
enableReset = lens getter setter
  where
    getter = getBool . _enableReset
    setter cfg bool = cfg { _enableReset = setBool (_enableReset cfg) bool }

backupBeforeClear :: Lens' Config Bool
backupBeforeClear = lens getter setter
  where
    getter = getBool . _backupBeforeClear
    setter cfg bool = cfg { _backupBeforeClear = setBool (_backupBeforeClear cfg) bool }

defaultBookmarks :: Lens' Config [DefaultBookmark]
defaultBookmarks = lens getter setter
  where
    getter :: Config -> [DefaultBookmark]
    getter = map getDefaultBookmark . getList . _defaultBookmarks

    setter :: Config -> [DefaultBookmark] -> Config
    setter cfg bms = cfg { _defaultBookmarks = setList (_defaultBookmarks cfg) (fmap DefaultBookmarkV bms)}

defaultCommand :: Lens' Config (Maybe Command)
defaultCommand = lens getter setter
  where
    getter :: Config -> Maybe Command
    getter = fmap getCommand . getMaybe . _defaultCommand

    setter :: Config -> Maybe Command -> Config
    setter cfg cmd = cfg { _defaultCommand = setMaybe (_defaultCommand cfg) (fmap CommandV cmd) }

-----------------------------------------

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
                         Just name  -> dir <> "\t(" <> name <> ")"

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

-- | Format a 'DefaultBookmark'. Used for the "config print" command and error reporting
-- during other commands.
--
-- @formatDefaultBookmark bm@ returns a pretty representation of @bm@.
formatDefaultBookmark :: DefaultBookmark -> T.Text
formatDefaultBookmark (DefaultBookmark dir mbName) =
  case mbName of Nothing    -> dir
                 Just name  -> dir <> "\t(" <> name <> ")"

-- | Show a value as a 'T.Text' instead of a 'String'.
tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | Format a config value. Used for the "config print" command.
formatConfigValue :: AnyConfigValue -> T.Text
formatConfigValue (AnyConfigValue (BoolV bool)) = tshow bool
formatConfigValue (AnyConfigValue (DefaultBookmarkV bm)) = formatDefaultBookmark bm
formatConfigValue (AnyConfigValue (CommandV t)) = tshow $ formatArgs $ formatCommand t
-- formatConfigValue (AnyConfigValue (CommandV t)) = tshow t
formatConfigValue (AnyConfigValue (MaybeV t)) = case t of
                                                  Nothing -> ""
                                                  Just t' -> formatConfigValue (AnyConfigValue t')
formatConfigValue (AnyConfigValue (ListOfV xs)) = T.intercalate "\n" (["["] <> map (("  " <>) . formatConfigValue . AnyConfigValue) xs <> ["]"])

formatBookmarkSearchTerm :: BookmarkSearchTerm -> T.Text
formatBookmarkSearchTerm (SearchIndex idx) = "#" <> tshow idx
formatBookmarkSearchTerm (SearchName name) = name

formatSearchTerm :: BookmarkSearchTerm -> T.Text
formatSearchTerm (SearchIndex idx) = tshow idx
formatSearchTerm (SearchName name) = name

singleton :: T.Text -> [T.Text]
singleton t = [t]

maybeSingleton :: Maybe T.Text -> [T.Text]
maybeSingleton = maybe [] singleton

maybeSingletonWithPrefix :: [T.Text] -> Maybe T.Text -> [T.Text]
maybeSingletonWithPrefix pref = maybe [] (\t -> pref <> [t])

formatCommand :: Command -> [T.Text]
formatCommand (Add (AddOptions d n)) = "add" : d : maybeSingleton n
formatCommand (Move opts) = ["move", formatSearchTerm $ moveSearch opts]
formatCommand (List (ListOptions n d)) = ["list"]
                                      <> maybeSingletonWithPrefix ["--name"] n
                                      <> maybeSingletonWithPrefix ["--dir"] d
formatCommand (Clear ClearOptions) = ["clear"]
formatCommand (Delete opts) = ["delete", formatSearchTerm $ deleteSearch opts]
formatCommand (Refresh RefreshOptions) = ["refresh"]
formatCommand (ConfigCmd cmd) = "config" : formatConfigCommand cmd
formatCommand (Check (CheckOptions c b)) = ["check"]
                                        <> (if c then ["--config"] else [])
                                        <> (if b then ["--bookmarks"] else [])
formatCommand DefaultCommand = []

formatConfigCommand :: ConfigCommand -> [T.Text]
formatConfigCommand (Print ConfigPrintOptions) = ["print"]
formatConfigCommand (Reset ConfigResetOptions) = ["reset"]
formatConfigCommand (Set opts) = ["set"
                                , setKey opts
                                , setValue opts
                                ]
formatConfigCommand (AddDefaultBookmark (ConfigAddDefaultOptions d n))
  = "add-default" : d : maybeSingleton n

formatGlobals :: GlobalOptions -> [T.Text]
formatGlobals (GlobalOptions c d o) = maybeSingletonWithPrefix ["--config-file"] c
                                   <> maybeSingletonWithPrefix ["--bookmarks-file"] d
                                   <> formatOverrides o

formatOverrides :: OverrideOptions -> [T.Text]
formatOverrides (OverrideOptions f t c r) = formatOverride "fail" "nofail" f
                                         <> formatOverride "time" "notime" t
                                         <> formatOverride "enable-clear" "disable-clear" c
                                         <> formatOverride "enable-reset" "disable-reset" r

formatOverride :: T.Text -> T.Text -> MaybeOverride -> [T.Text]
formatOverride _ no OverrideFalse = ["--" <> no]
formatOverride yes _ OverrideTrue = ["--" <> yes]
formatOverride _ _ NoOverride     = []
formatOverride yes no Conflict    = ["--" <> no, "--" <> yes]

formatOptions :: Options -> [T.Text]
formatOptions (Options c g) = formatCommand c <> formatGlobals g

formatArgs :: [T.Text] -> T.Text
formatArgs = T.unwords . map quoteStrings
  where quoteStrings :: T.Text -> T.Text
        quoteStrings s | T.elem ' ' s = "\"" <> s <> "\"" | otherwise  = s
