{-|
Module      : HoYo.Internal.Command
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

Internals used by the HoYo.Command module.
-}

module HoYo.Internal.Command where

import HoYo.Bookmark
import HoYo.Config
import HoYo.Internal.Types
import HoYo.Internal.Utils

import Data.Char (isDigit)
import Data.Function
import Data.List

import Control.Applicative

import qualified Data.Text as T

import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (ask)

import Lens.Micro
import Lens.Micro.Extras

import System.Directory
import System.Exit

import Data.Time

-- | Options for the "add" command to be parsed from the command-line.
data AddOptions = AddOptions {
  addDirectory  :: TFilePath
  , addName     :: Maybe T.Text
  } deriving Show

-- | Options for the "move" command to be parsed from the command-line.
newtype MoveOptions = MoveOptions {
  moveSearch :: BookmarkSearchTerm
  } deriving Show

-- | Options for the "list" command to be parsed from the command-line.
data ListOptions = ListOptions {
  listFilterName                :: Maybe T.Text
  , listFilterDirectoryInfix    :: Maybe T.Text
  } deriving Show

-- | Options for the "clear" command to be parsed from the command-line.
data ClearOptions = ClearOptions {
  } deriving Show

-- | Options for the "delete" command to be parsed from the command-line.
newtype DeleteOptions = DeleteOptions {
  deleteSearch :: BookmarkSearchTerm
  } deriving Show

-- | Options for the "refresh" command to be parsed from the command-line.
data RefreshOptions = RefreshOptions {
  } deriving Show

-- | Options for the "config print" command to be parsed from the command-line.
data ConfigPrintOptions = ConfigPrintOptions {
  } deriving Show

-- | Options for the "config reset" command to be parsed from the command-line.
data ConfigResetOptions = ConfigResetOptions {
  } deriving Show

-- | Options for the "config set" command to be parsed from the command-line.
data ConfigSetOptions = ConfigSetOptions {
  setKey        :: T.Text
  , setValue    :: T.Text
  } deriving Show

-- | Options for the "config add-default" command to be parsed from the command-line.
data ConfigAddDefaultOptions = ConfigAddDefaultOptions {
  addDefaultDir       :: TFilePath
  , addDefaultName    :: Maybe T.Text
  } deriving Show

-- | Options for the "config" command to be parsed from the command-line.
data ConfigCommand =
  Print ConfigPrintOptions
  | Reset ConfigResetOptions
  | Set ConfigSetOptions
  | AddDefaultBookmark ConfigAddDefaultOptions
  deriving Show

-- | Options for the "check" command to be parsed from the command-line.
data CheckOptions = CheckOptions {
  checkConfig         :: Bool
  , checkBookmarks    :: Bool
  } deriving Show

-- | The core data-type for the hoyo CLI. The 'Command' is parsed from the command-line,
-- then 'runCommand' dispatches on the type.
data Command =
  Add AddOptions
  | Move MoveOptions
  | List ListOptions
  | Clear ClearOptions
  | Delete DeleteOptions
  | Refresh RefreshOptions
  | ConfigCmd ConfigCommand
  | Check CheckOptions
  | DefaultCommand
  deriving Show

-- | Datatype for representing a command-line settings override.
data MaybeOverride =
  OverrideFalse
  | OverrideTrue
  | NoOverride
  | Conflict
  deriving Show

-- | Combine a config flag with a command-line flag, checking for conflicts.
combOverride :: Bool -> Bool -> MaybeOverride
combOverride False False = NoOverride
combOverride True  False = OverrideTrue
combOverride False True  = OverrideFalse
combOverride True  True  = Conflict

-- | Config settings that can be overriden using command-line flags.
data OverrideOptions = OverrideOptions {
  overrideFailOnError               :: MaybeOverride
  , overrideDisplayCreationTime     :: MaybeOverride
  , overrideEnableClearing          :: MaybeOverride
  , overrideEnableReset             :: MaybeOverride
  } deriving Show

-- | Convert a 'MaybeOverride' to a function on 'Bool'.
overrideFunc :: MaybeOverride -> (Bool -> Bool)
overrideFunc NoOverride     = id
overrideFunc OverrideTrue   = const True
overrideFunc OverrideFalse  = const False
overrideFunc Conflict       = error "override conflict!"

-- | Apply the override options to a 'Config'.
overrideConfig :: OverrideOptions -> Config -> Config
overrideConfig opts =
  over failOnError            (overrideFunc $         overrideFailOnError opts)
  . over displayCreationTime  (overrideFunc $ overrideDisplayCreationTime opts)
  . over enableClearing       (overrideFunc $      overrideEnableClearing opts)
  . over enableReset          (overrideFunc $         overrideEnableReset opts)

-- | Apply the override options to an 'Env'.
overrideEnv :: OverrideOptions -> Env -> Env
overrideEnv = over config . overrideConfig

-- | Check that there are no conflicting overrides.
verifyOverrides :: OverrideOptions -> Maybe T.Text
verifyOverrides (OverrideOptions o1 o2 o3 o4) = verify o1
                                                  <|> verify o2
                                                  <|> verify o3
                                                  <|> verify o4
  where verify Conflict = Just "conflicting flags"
        verify _ = Nothing

-- | CLI options that can be set regardless of which command is run.
data GlobalOptions = GlobalOptions {
  globalConfigPath  :: Maybe TFilePath
  , dataPath        :: Maybe TFilePath
  , overrides       :: OverrideOptions
  } deriving Show

-- | The default behaviour is to override nothing.
defaultOverrideOptions :: OverrideOptions
defaultOverrideOptions = OverrideOptions NoOverride NoOverride NoOverride NoOverride

-- | Default global options. In general this should do nothing.
defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions Nothing Nothing defaultOverrideOptions

-- | The final result of parsing the CLI arguments. Contains a command and all
-- information for that command, and any global options that have been set.
data Options = Options {
  optCommand    :: Command
  , optGlobals  :: GlobalOptions
  } deriving Show

-- | Helper function whenever we need to modify the saved bookmarks.
--
-- @modifyBookmarks f@ retrieves the current bookmarks, applies @f@,
-- and saves them back to file.
modifyBookmarks :: ([Bookmark] -> [Bookmark]) -> HoYoMonad ()
modifyBookmarks f = modifyBookmarksM (return . f)

-- | Helper function twhenever we need to modify the saved bookmarks,
-- and need access to the hoyo environment.
--
-- @modifyBookmarks f@ retrieves the current bookmarks, applies @f@
-- in the hoyo environment, and saves them back to file.
modifyBookmarksM :: ([Bookmark] -> HoYoMonad [Bookmark]) -> HoYoMonad ()
modifyBookmarksM f = do
  Env (Bookmarks bms) bFp _ _ <- ask
  newBookmarks <- Bookmarks <$> f bms
  encodeBookmarksFile bFp newBookmarks

-- | Normalise a filepath and make sure it's a valid directory.
normaliseAndVerifyDirectory :: TFilePath -> HoYoMonad TFilePath
normaliseAndVerifyDirectory d = do
  dir <- liftIO $ canonicalizePath $ T.unpack d
  assertVerbose "not a directory" $ liftIO $ doesDirectoryExist dir
  return $ T.pack dir

-- | Take a name and make sure it's valid.
verifyName :: T.Text -> HoYoMonad ()
verifyName name = do
  let nameStr = T.unpack name
  assert "bookmark name can't be empty" $ return $ not $ T.null name
  assert "bookmark name can't be a number" $ return $ not $ all isDigit nameStr

-- | Given the existing bookmarks and a potential bookmark name,
-- test if the new bookmark will have a unique name.
testNameUnique :: [Bookmark] -> T.Text -> HoYoMonad Bool
testNameUnique bms name =
  assertVerbose "bookmark name already used" $
    return $ all ((/= Just name) . view bookmarkName) bms

-- | Run the "add" command: add a new bookmark.
runAdd :: AddOptions -> HoYoMonad ()
runAdd opts = do
  dir <- normaliseAndVerifyDirectory $ addDirectory opts
  let name = addName opts
  modifyBookmarksM $ \bms -> do
    uniqName <- testNameUnique bms dir
    if uniqName
    then do
      let maxIndex = maximumDefault 0 $ map (view bookmarkIndex) bms
      zTime <- liftIO getZonedTime
      let newBookMark = Bookmark dir (maxIndex + 1) zTime name
      return (newBookMark : bms)
    else return bms

-- | Run the "move" command: search for a bookmark and @cd@ to it.
runMove :: MoveOptions -> HoYoMonad ()
runMove opts = do
  bms <- asks' bookmarks
  let search = moveSearch opts
  case fst $ searchBookmarks search bms of
    []    -> throwError ("Unknown bookmark: " <> tshow search)
    [bm]  -> do printStdout ("cd " <> view bookmarkDirectory bm)
                liftIO $ exitWith (ExitFailure 3)
    ms    -> do displayTime <- asks' (config . displayCreationTime)
                let strs = formatBookmarks displayTime $ sortOn (view bookmarkIndex) ms
                throwError $ T.intercalate "\n" ("multiple bookmarks matching search:" : strs)

-- | Run the "list" command: list all the saved bookmarks.
runList :: ListOptions -> HoYoMonad ()
runList opts = do
  let filt = filterBookmarks (listFilterName opts) (listFilterDirectoryInfix opts)
  bms' <- asks' bookmarks
  let bms = filter filt $ sortOn (view bookmarkIndex) $ unBookmarks bms'
  displayTime <- asks' (config . displayCreationTime)
  mapM_ printStdout (formatBookmarks displayTime bms)

-- | Help text displayed when the user tries to run "hoyo clear"
-- when "enable_clear" is set to false.
clearDisabledErrMsg :: T.Text
clearDisabledErrMsg = T.intercalate "\n" [
  "The 'clear' command is disabled by default."
  , "To enable, set enable_clear = true in the config or pass the --enable-clear flag."
  ]

-- | Help text displayed when the user tries to run "hoyo config reset"
-- when "enable_reset" is set to false.
resetDisabledErrMsg :: T.Text
resetDisabledErrMsg = T.intercalate "\n" [
  "The 'config reset' command is disabled by default."
  , "To enable, set enable_reset = true in the config or pass the --enable-reset flag."
  ]

-- | Run the "clear" command: delete all the saved bookmarks.
runClear :: ClearOptions -> HoYoMonad ()
runClear _ = do
  assert clearDisabledErrMsg (asks' (config . enableClearing))

  path <- asks' bookmarksPath
  backup <- asks' (config . backupBeforeClear)
  when backup $ backupFile path "bkp"

  modifyBookmarks $ const []
  bms <- asks' (config . defaultBookmarks) >>= bookmarksFromDefault
  fp <- asks' bookmarksPath
  encodeBookmarksFile fp bms

-- | Run the "delete" command: search for a bookmark and delete it.
runDelete :: DeleteOptions -> HoYoMonad ()
runDelete opts = do
  let search = deleteSearch opts
  modifyBookmarksM $ \bms -> do
    let (searchResults, afterDelete) = searchBookmarks search (Bookmarks bms)
    assertVerbose ("no bookmarks found for search " <> tshow search)
      $ return $ not $ null searchResults
    assertVerbose ("multiple bookmarks found for search " <> tshow search)
      $ return $ length searchResults == 1
    return afterDelete

-- | Run the "refresh" command: re-index bookmarks.
runRefresh :: RefreshOptions -> HoYoMonad ()
runRefresh _ = modifyBookmarks $
  zipWith (set bookmarkIndex) [1..]                           -- re-index from 1
    . nubBy ((==) `on` view bookmarkDirectory)                -- remove duplicate directories
    . sortOn (zonedTimeToUTC . view bookmarkCreationTime)     -- sort by creation time

-- | Run the "config print" command: print the current config.
runConfigPrint :: ConfigPrintOptions -> HoYoMonad ()
runConfigPrint _ = do
  s <- asks' config
  let keyVals = getKeyVals s
  let keyWidth = maximum $ map (T.length . fst) keyVals
  forM_ keyVals $ \(key, val) -> do
    let vStr = formatConfigValue val
    printStdout (T.justifyLeft keyWidth ' ' key <> " = " <> vStr)

-- | Run the "config reset" command: reset the config to 'defaultConfig'.
runConfigReset :: ConfigResetOptions -> HoYoMonad ()
runConfigReset _ = do
  assert resetDisabledErrMsg (asks' (config . enableReset))

  path <- asks' configPath

  backup <- asks' (config . backupBeforeClear)
  when backup $ backupFile path "bkp"

  encodeConfigFile path defaultConfig

-- | Run the "config set" command: try to set a key-value pair in the config.
runConfigSet :: ConfigSetOptions -> HoYoMonad ()
runConfigSet opts = do
  let key = setKey opts
  let val = setValue opts
  cfgPath <- asks' configPath
  asks' config
    >>= setConfig key val
    >>= encodeConfigFile cfgPath

-- | Run the "config add-default" command: try to set a key-value pair in the config.
runAddDefault :: ConfigAddDefaultOptions -> HoYoMonad ()
runAddDefault opts = do
  dir <- normaliseAndVerifyDirectory $ addDefaultDir opts

  let name = addDefaultName opts
  let defaultBm = DefaultBookmark dir name
  cfgPath <- asks' configPath
  asks' config
    >>= encodeConfigFile cfgPath . over defaultBookmarks (defaultBm :)

-- | Run the "config" command: dispatch on the given sub-command.
runConfig :: ConfigCommand -> HoYoMonad ()
runConfig (Print opts) = runConfigPrint opts
runConfig (Reset opts) = runConfigReset opts
runConfig (Set opts) = runConfigSet opts
runConfig (AddDefaultBookmark opts) = runAddDefault opts

-- | Check that the config file is valid.
runCheckConfig :: TFilePath -> IO ()
runCheckConfig = decodeConfigFile >=> \case
  Left err  -> printStderr err
  Right _   -> printStdout "Config is good"

-- | Check that the bookmarks file is valid.
runCheckBookmarks :: TFilePath -> IO ()
runCheckBookmarks = decodeBookmarksFile >=> \case
  Left err  -> printStderr err
  Right _   -> printStdout "Bookmarks file is good"

-- | Run the "config check" command: validate the current
-- config and bookmarks files.
runCheck :: CheckOptions -> TFilePath -> TFilePath -> IO ()
runCheck opts bFp sFp = do
  when (checkConfig opts) $ runCheckConfig sFp
  when (checkBookmarks opts) $ runCheckBookmarks bFp

-- | Run the default command, if it has been specified by the user.
runDefaultCommand :: HoYoMonad ExecResult
runDefaultCommand = asks' (config . defaultCommand) >>= \case
  Nothing   -> return ShowHelp
  Just cmd
    | null $ T.words cmd  -> throwError "default command: stuck in a loop!"
    | otherwise           -> return $ ReRun cmd

-- | Run a 'Command' in the hoyo environment.
runCommand :: Command -> HoYoMonad ()
runCommand       (Add opts) = runAdd opts
runCommand      (Move opts) = runMove opts
runCommand      (List opts) = runList opts
runCommand     (Clear opts) = runClear opts
runCommand    (Delete opts) = runDelete opts
runCommand   (Refresh opts) = runRefresh opts
runCommand (ConfigCmd opts) = runConfig opts
runCommand   DefaultCommand = void runDefaultCommand
runCommand        (Check _) = throwError "The 'check' command needs to be run outside the HoYo monad"
