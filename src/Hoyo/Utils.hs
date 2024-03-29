{-|
Module      : Hoyo.Utils
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Utility functions used by all the main Hoyo.* modules.
-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Hoyo.Utils (
  -- * Lenses for ConfigValue
    cfgBool
  , cfgDefaultBookmark
  , cfgCommand
  , cfgList
  , cfgMaybe

  -- * Lenses for Config
  , failOnError
  , displayCreationTime
  , enableClearing
  , enableReset
  , backupBeforeClear
  , defaultBookmarks
  , defaultCommand

  -- * Utility functions
  , asks'
  , assert
  , assertVerbose
  , maximumDefault
  , catchIOException

  -- ** Backups
  , backupFile

  -- ** Parsing functions
  , readBool
  , readInt

  -- ** Printing functions
  , printStdout
  , printStderr
  , pageLines

  -- ** Formatting functions
  , formatArgs
  , formatCommand
  , formatBookmark
  , formatBookmarks
  , formatConfigValue
  , formatOptions
  , formatException
  , tshow
  , anyCfgValToJson
  , bookmarksToJSON
  ) where

{- HLINT ignore "Use list comprehension" -}

import           Control.Applicative
import           Control.Exception          (IOException, bracket_)
import           Control.Monad              (unless, when)
import           Control.Monad.Except
                 ( MonadError (..)
                 , liftEither
                 , throwError
                 )
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class (MonadReader, asks)

import           Data.Bifunctor             (bimap, first)
import           Data.Foldable              (toList)
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time

import           Hoyo.Internal.Types

import           Lens.Micro
import           Lens.Micro.Extras

import           System.Console.ANSI        hiding (Reset)
import           System.Directory
import           System.IO
import           System.Pager

import           Text.JSON
import           Text.Read                  (readEither)

import qualified Toml.Parser.Core           as Toml
                 ( eof
                 , errorBundlePretty
                 , parse
                 )
import qualified Toml.Parser.Value          as Toml


-----------------------------------------
-- getters and setter for ConfigValue

-- | A lens into a boolean config value.
cfgBool :: Lens' (ConfigValue 'TBool) Bool
cfgBool = lens getBool setBool
  where
    getBool :: ConfigValue 'TBool -> Bool
    getBool (BoolV bool) = bool

    setBool :: ConfigValue 'TBool -> Bool -> ConfigValue 'TBool
    setBool _ = BoolV

-- | A lens into a default bookmark config value.
cfgDefaultBookmark :: Lens' (ConfigValue 'TDefaultBookmark) DefaultBookmark
cfgDefaultBookmark = lens getDefaultBookmark setDefaultBookmark
  where
    getDefaultBookmark :: ConfigValue 'TDefaultBookmark -> DefaultBookmark
    getDefaultBookmark (DefaultBookmarkV bm) = bm

    setDefaultBookmark :: ConfigValue 'TDefaultBookmark -> DefaultBookmark -> ConfigValue 'TDefaultBookmark
    setDefaultBookmark _ = DefaultBookmarkV

-- | A lens into a command config value.
cfgCommand :: Lens' (ConfigValue 'TCommand) Command
cfgCommand = lens getCommand setCommand
  where
    getCommand :: ConfigValue 'TCommand -> Command
    getCommand (CommandV t) = t

    setCommand :: ConfigValue 'TCommand -> Command -> ConfigValue 'TCommand
    setCommand _ = CommandV

-- | A lens into a list config value.
cfgList :: Lens' (ConfigValue ('TList t)) [ConfigValue t]
cfgList = lens getList setList
  where
    getList :: ConfigValue ('TList t) -> [ConfigValue t]
    getList (ListOfV xs) = xs

    setList :: ConfigValue ('TList t) -> [ConfigValue t] -> ConfigValue ('TList t)
    setList _ = ListOfV

-- | A lens into an optional config value.
cfgMaybe :: Lens' (ConfigValue ('TMaybe t)) (Maybe (ConfigValue t))
cfgMaybe = lens getMaybe setMaybe
  where
    getMaybe :: ConfigValue ('TMaybe t) -> Maybe (ConfigValue t)
    getMaybe (MaybeV val) = val

    setMaybe :: ConfigValue ('TMaybe t) -> Maybe (ConfigValue t) -> ConfigValue ('TMaybe t)
    setMaybe _ = MaybeV

-----------------------------------------
-- Lenses for Config

failOnError :: Lens' Config Bool
failOnError = __failOnError . cfgBool

displayCreationTime :: Lens' Config Bool
displayCreationTime = __displayCreationTime . cfgBool

enableClearing :: Lens' Config Bool
enableClearing = __enableClearing . cfgBool

enableReset :: Lens' Config Bool
enableReset = __enableReset . cfgBool

backupBeforeClear :: Lens' Config Bool
backupBeforeClear = __backupBeforeClear . cfgBool

liftLensToList :: Lens' a b -> Lens' [a] [b]
liftLensToList l = zipList' . liftLens l . zipList
  where
    zipList = lens getZipList const
    zipList' = lens ZipList const

liftLens :: Applicative g => Lens' a b -> Lens' (g a) (g b)
liftLens l = lens (fmap (view l)) (\ga gb -> set l <$> gb <*> ga)

defaultBookmarks :: Lens' Config [DefaultBookmark]
defaultBookmarks = __defaultBookmarks . cfgList . liftLensToList cfgDefaultBookmark

defaultCommand :: Lens' Config (Maybe Command)
defaultCommand = __defaultCommand . cfgMaybe . liftLens cfgCommand

-----------------------------------------

-- | A version of the lens "use" function for 'MonadReader'.
asks' :: MonadReader a m => SimpleGetter a b -> m b
asks' = asks . view

-- | Take the maximum of a list, with a default value if the list is empty.
maximumDefault :: Ord a => a -> [a] -> a
maximumDefault def [] = def
maximumDefault _ xs   = maximum xs

-- | Throw an error if a check fails.
assert :: HoyoException -> HoyoMonad Bool -> HoyoMonad ()
assert err check = do
  res <- check
  unless res $ throwError err

-- | Throw an error if a check fails AND the "fail_on_error" flag is set.
assertVerbose :: HoyoException -> HoyoMonad Bool -> HoyoMonad Bool
assertVerbose err check = do
  shouldFail <- asks' (config . failOnError)
  res <- check
  when (shouldFail && not res) $ throwError err
  return res

-- | Given a file name and an extension, try to find a suitable path for
-- backing up that file. Used by 'backupFile'.
getBackupFile :: (MonadIO m, MonadError HoyoException m) => FilePath -> String -> m FilePath
getBackupFile fp ext = do
  ex <- liftIO $ doesFileExist fp
  unless ex $ throwError $ FileSystemException $ NoFileException fp
  let firstTry = fp <> "." <> ext
  firstExists <- liftIO $ doesFileExist firstTry
  if firstExists
  then getBackupFile' fp 2
  else return firstTry

  where
    getBackupFile' :: (MonadIO m, MonadError HoyoException m) => String -> Int -> m String
    getBackupFile' file' n = do
      let file = file' <> "." <> show n <> ext
      fileExists <- liftIO $ doesFileExist file
      if fileExists
      then getBackupFile' file' (n + 1)
      else return file

-- | Try to back-up a file. Used when the "backup_before_clear" option is set.
backupFile :: (MonadIO m, MonadError HoyoException m) => FilePath -> String -> m ()
backupFile fp ext = do
  file <- getBackupFile fp ext
  liftIO $ copyFileWithMetadata fp file

-- | Try to read a 'Bool'.
readBool :: MonadError HoyoException m => T.Text -> m Bool
readBool s = liftEither $ first (ParseException . pure . T.pack) (
              readEither sStr
                <|> first Toml.errorBundlePretty (Toml.parse (Toml.boolP <* Toml.eof) "" s)
                <|> Left ("Couldn't parse bool: " <> sStr)
              )
  where sStr = T.unpack s

-- | Try to read an 'Int'.
readInt :: MonadError HoyoException m => T.Text -> m Int
readInt s = liftEither $ first (ParseException . pure . T.pack) (
              readEither sStr
                <|> bimap Toml.errorBundlePretty fromIntegral (Toml.parse (Toml.integerP <* Toml.eof) "" s)
                <|> Left ("Couldn't parse integer: " <> sStr)
              )
  where sStr = T.unpack s

-- | Print to stderr.
printStderr :: MonadIO m => T.Text -> m ()
printStderr msg = liftIO $ bracket_ makeRed resetColour $ T.hPutStrLn stderr msg
  where
    makeRed = hSetSGR stderr [SetColor Foreground Vivid Red]
    resetColour = do
      hSetSGR stderr []
      hPutStrLn stderr ""

-- | Print to stdout.
printStdout :: MonadIO m => T.Text -> m ()
printStdout = liftIO . T.putStrLn

-- | Page lines if larger than one page and if the output device is a terminal.
-- Otherwise, print.
pageLines :: MonadIO m => [T.Text] -> m ()
pageLines ts = do
  let t = T.intercalate "\n" ts
  isTTY <- liftIO $ hIsTerminalDevice stdin
  if isTTY
  then liftIO $ printOrPage (t <> "\n")
  else printStdout t

-- | Format a 'Bookmark'. Used for the "list" command and error reporting
-- during other commands.
--
-- @formatBookmark displayTime numberWidth bm@ returns a pretty representation
-- of @bm@, optionally showing the creation time, and padding the index and
-- directory to a certain width.
formatBookmark :: Bool -> Int -> Int -> Bookmark -> T.Text
formatBookmark shouldDisplayTime indexWidth direcWidth (Bookmark dir idx zTime mbName) =
  let num = T.justifyRight indexWidth ' ' $ tshow idx
      dirStr = T.justifyLeft direcWidth ' ' $ T.pack dir
      timeStr = T.pack $ formatTime defaultTimeLocale "%D %T" zTime
      d = case mbName of Nothing   -> dirStr
                         Just name -> dirStr <> "  (" <> name <> ")"

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
formatBookmarks shouldDisplayTime bms = map (formatBookmark shouldDisplayTime indexWidth direcWidth) bms
  where
    indexWidth = maximumDefault 1 $ map (length . show . view bookmarkIndex) bms
    direcWidth = maximumDefault 1 $ map (length . view bookmarkDirectory) bms

-- | Format a 'DefaultBookmark'. Used for the "config print" command and error reporting
-- during other commands.
--
-- @formatDefaultBookmark bm@ returns a pretty representation of @bm@.
formatDefaultBookmark :: DefaultBookmark -> T.Text
formatDefaultBookmark (DefaultBookmark dir mbName) =
  case mbName of Nothing   -> T.pack dir
                 Just name -> T.pack dir <> "\t(" <> name <> ")"

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

-- formatBookmarkSearchTerm :: BookmarkSearchTerm -> T.Text
-- formatBookmarkSearchTerm (SearchIndex idx) = "#" <> tshow idx
-- formatBookmarkSearchTerm (SearchName name) = name

formatSearchTerm :: BookmarkSearchTerm -> T.Text
formatSearchTerm (SearchIndex idx) = tshow idx
formatSearchTerm (SearchName name) = name

formatSearchTermPretty :: BookmarkSearchTerm -> T.Text
formatSearchTermPretty (SearchIndex idx) = "#" <> tshow idx
formatSearchTermPretty (SearchName name) = name

singleton :: T.Text -> [T.Text]
singleton t = [t]

maybeSingleton :: Maybe T.Text -> [T.Text]
maybeSingleton = maybe [] singleton

maybeSingletonWithPrefix :: [T.Text] -> Maybe T.Text -> [T.Text]
maybeSingletonWithPrefix pref = maybe [] (\t -> pref <> [t])

-- | Format a 'Command' in the same way it would be parsed from the command line.
formatCommand :: Command -> [T.Text]
formatCommand (Add (AddOptions d n)) = "add" : T.pack d : maybeSingleton n
formatCommand (Move opts) = ["move", formatSearchTerm $ moveSearch opts]
formatCommand (List (ListOptions n d json)) = ["list"]
                                           <> maybeSingletonWithPrefix ["--name"] n
                                           <> maybeSingletonWithPrefix ["--dir"] d
                                           <> (if json then ["--json"] else [])
formatCommand (Clear ClearOptions) = ["clear"]
formatCommand (Delete opts) = ["delete", formatSearchTerm $ deleteSearch opts]
formatCommand (Refresh RefreshOptions) = ["refresh"]
formatCommand (ConfigCmd cmd) = "config" : formatConfigCommand cmd
formatCommand (Check (CheckOptions c b)) = ["check"]
                                        <> (if c then ["--config"] else [])
                                        <> (if b then ["--bookmarks"] else [])
formatCommand (Help (HelpOptions cmd)) = ["help"] <> maybeToList cmd
formatCommand DefaultCommand = []

formatConfigCommand :: ConfigCommand -> [T.Text]
formatConfigCommand (Print (ConfigPrintOptions json)) = ["print"]
                                                     <> (if json then ["--json"] else [])
formatConfigCommand (Reset ConfigResetOptions) = ["reset"]
formatConfigCommand (Set opts) = ["set"
                                , setKey opts
                                , setValue opts
                                ]
formatConfigCommand (AddDefaultBookmark (ConfigAddDefaultOptions d n))
  = "add-default" : T.pack d : maybeSingleton n

formatGlobals :: GlobalOptions -> [T.Text]
formatGlobals (GlobalOptions c d o) = maybeSingletonWithPrefix ["--config-file"] (T.pack <$> c)
                                   <> maybeSingletonWithPrefix ["--bookmarks-file"] (T.pack <$> d)
                                   <> formatOverrides o

formatOverrides :: OverrideOptions -> [T.Text]
formatOverrides (OverrideOptions f t c r b) = formatOverride "fail" "nofail" f
                                           <> formatOverride "time" "notime" t
                                           <> formatOverride "enable-clear" "disable-clear" c
                                           <> formatOverride "enable-reset" "disable-reset" r
                                           <> formatOverride "backup-before-clear" "no-backup-before-clear" b

formatOverride :: T.Text -> T.Text -> MaybeOverride -> [T.Text]
formatOverride _ no OverrideFalse = ["--" <> no]
formatOverride yes _ OverrideTrue = ["--" <> yes]
formatOverride _ _ NoOverride     = []
formatOverride yes no Conflict    = ["--" <> no, "--" <> yes]

-- | Format an 'Options' object in the same way it would be parsed from the command line.
formatOptions :: Options -> [T.Text]
formatOptions (Options c g) = formatCommand c <> formatGlobals g

-- | Format a list of arguments into a single 'T.Text', enclosing multi-word arguments in quotes.
formatArgs :: [T.Text] -> T.Text
formatArgs = T.unwords . map quoteStrings
  where quoteStrings :: T.Text -> T.Text
        quoteStrings s | ' ' `elem` T.unpack s = "\"" <> s <> "\"" | otherwise  = s

-- | Convert a list of 'Bookmark's to a JSON value. Used in `hoyo list --json`.
bookmarksToJSON :: Bool -> [Bookmark] -> JSValue
bookmarksToJSON displayTime bms =
  let nameObj = maybe JSNull (JSString . toJSString . T.unpack)
      bmToObj (Bookmark dir idx time mbName) = JSObject $ toJSObject $ [
          ("index", JSRational False $ toRational idx)
        , ("directory", JSString $ toJSString dir)
        , ("name", nameObj mbName)
        ]
        <> [("creation_time", JSString $ toJSString $ formatTime defaultTimeLocale "%c" time)
            | displayTime]
      arr = JSArray $ map bmToObj bms
  in JSObject $ toJSObject [("bookmarks", arr)]

cfgValToJson :: forall (t :: ConfigValueType). ConfigValue t -> JSValue
cfgValToJson (BoolV b) = JSBool b
cfgValToJson (DefaultBookmarkV (DefaultBookmark dir mbName)) =
  JSObject $ toJSObject [
      ("directory", JSString $ toJSString dir)
    , ("name", case mbName of Nothing   -> JSNull
                              Just name -> JSString $ toJSString $ T.unpack name)
    ]
cfgValToJson (CommandV cmd) = JSString $ toJSString $ T.unpack $ T.unwords $ formatCommand cmd
cfgValToJson (ListOfV xs) = JSArray $ map cfgValToJson xs
cfgValToJson (MaybeV Nothing) = JSNull
cfgValToJson (MaybeV (Just x)) = cfgValToJson x

-- | Convert a configuration value to a JSON value. Used in `hoyo config print --json`.
anyCfgValToJson :: AnyConfigValue -> JSValue
anyCfgValToJson (AnyConfigValue val) = cfgValToJson val

withTitle :: T.Text -> [T.Text] -> T.Text
withTitle title []    = title
withTitle title [one] = title <> ": " <> one
withTitle title ts    = T.intercalate "\n" $ (title <> ":"): map ("  " <>) ts

formatFsException :: FileSystemException -> T.Text
formatFsException (NoFileException fp) = withTitle "file not found" [T.pack fp]
formatFsException (NoDirException fp) = withTitle "directory not found" [T.pack fp]

formatCmdException :: CommandException -> T.Text
formatCmdException (SearchException (NothingFound search)) = withTitle "unknown bookmark" [formatSearchTermPretty search]
formatCmdException (SearchException (TooManyResults search bms)) = withTitle ("multiple bookmarks matching search [" <> tshow search <> "]") bms
formatCmdException (InvalidArgumentException ts) = withTitle "invalid argument(s)" ts
formatCmdException LoopException = withTitle "default command" ["stuck in a loop"]

-- | Format a 'HoyoException' to display to the user.
formatException :: HoyoException -> T.Text
formatException (ConfigException ts) = withTitle "config error" ts
formatException (CommandException cmdExc) = formatCmdException cmdExc
formatException (IOException ioExc) = withTitle "IO error:" [tshow ioExc]
formatException (FileSystemException exc) = formatFsException exc
formatException (ParseException ts) = withTitle "parse error" ts
formatException (MultipleExceptions excs) = T.intercalate "\n" $ map formatException $ toList excs

-- | Catch an 'GHC.IO.Exception.IOException' and wrap it in a 'HoyoException'.
catchIOException :: Monad m => IOException -> m (Either HoyoException a)
catchIOException exc = return $ Left $ IOException exc
