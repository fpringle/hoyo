-- | This module defines data-types and runner functions for the hoyo
-- command-line program.
module HoYo.Command (
  -- * Running CLI commands
  runCommand
  , modifyBookmarks
  , modifyBookmarksM

  -- ** Specific command runners
  , runAdd
  , runMove
  , runList
  , runClear
  , runDelete
  , runRefresh
  , runConfig

  -- * Types
  , Options (..)
  , Command (..)
  , AddOptions (..)
  , MoveOptions (..)
  , ListOptions (..)
  , ClearOptions (..)
  , DeleteOptions (..)
  , RefreshOptions (..)
  , ConfigPrintOptions (..)
  , ConfigResetOptions (..)
  , ConfigSetOptions (..)
  , ConfigCommand (..)
  , GlobalOptions (..)
  , OverrideOptions (..)
  , overrideConfig
  , overrideEnv
  , verifyOverrides
  , combOverride
  , MaybeOverride (..)
  ) where

import HoYo.Bookmark
import HoYo.Config
import HoYo.Types
import HoYo.Utils

import Data.Function
import Data.List

import Control.Applicative

import qualified Data.Text as T

import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (ask)

import Lens.Simple

import System.Directory
import System.Exit

import Data.Time

import qualified Toml

-- | Options for the "add" command to be parsed from the command-line.
data AddOptions = AddOptions {
  addDirectory  :: FilePath
  , addName     :: Maybe String
  }

-- | Options for the "move" command to be parsed from the command-line.
newtype MoveOptions = MoveOptions {
  moveSearch :: BookmarkSearchTerm
  }

-- | Options for the "list" command to be parsed from the command-line.
data ListOptions = ListOptions {
  listFilterName                :: Maybe String
  , listFilterDirectoryInfix    :: Maybe String
  }

-- | Options for the "clear" command to be parsed from the command-line.
data ClearOptions = ClearOptions {
  }

-- | Options for the "delete" command to be parsed from the command-line.
newtype DeleteOptions = DeleteOptions {
  deleteSearch :: BookmarkSearchTerm
  }

-- | Options for the "refresh" command to be parsed from the command-line.
data RefreshOptions = RefreshOptions {
  }

-- | Options for the "config print" command to be parsed from the command-line.
data ConfigPrintOptions = ConfigPrintOptions {
  }

-- | Options for the "config reset" command to be parsed from the command-line.
data ConfigResetOptions = ConfigResetOptions {
  }

-- | Options for the "config set" command to be parsed from the command-line.
data ConfigSetOptions = ConfigSetOptions {
  setKey        :: String
  , setValue    :: String
  }

-- | Options for the "config" command to be parsed from the command-line.
data ConfigCommand =
  Print ConfigPrintOptions
  | Reset ConfigResetOptions
  | Set ConfigSetOptions

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

-- | Datatype for representing a command-line settings override.
data MaybeOverride =
  OverrideFalse
  | OverrideTrue
  | NoOverride
  | Conflict

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
  }

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
verifyOverrides :: OverrideOptions -> Maybe String
verifyOverrides (OverrideOptions o1 o2 o3 o4) = verify o1
                                                  <|> verify o2
                                                  <|> verify o3
                                                  <|> verify o4
  where verify Conflict = Just "conflicting flags"
        verify _ = Nothing

-- | CLI options that can be set regardless of which command is run.
data GlobalOptions = GlobalOptions {
  globalConfigPath  :: Maybe FilePath
  , dataPath        :: Maybe FilePath
  , overrides       :: OverrideOptions
  }

-- | The final result of parsing the CLI arguments. Contains a command and all
-- information for that command, and any global options that have been set.
data Options = Options {
  optCommand    :: Command
  , optGlobals  :: GlobalOptions
  }

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

-- | Run the "add" command: add a new bookmark.
runAdd :: AddOptions -> HoYoMonad ()
runAdd opts = do
  -- TODO: verify the nickname isn't a number (messes up parsing for search)
  dir <- liftIO $ canonicalizePath (addDirectory opts)
  void $ assertVerbose "not a directory" $ liftIO $ doesDirectoryExist dir
  modifyBookmarksM $ \bms -> do
    -- TODO: also change nickname is unique
    uniq <- assertVerbose "bookmark already exists" $
      return $ all ((/= dir) . view bookmarkDirectory) bms
    if uniq
    then do
      let maxIndex = maximumDefault 0 $ map (view bookmarkIndex) bms
      zTime <- liftIO getZonedTime
      let newBookMark = Bookmark dir (maxIndex + 1) zTime (addName opts)
      return (newBookMark : bms)
    else return bms

-- | Run the "move" command: search for a bookmark and @cd@ to it.
runMove :: MoveOptions -> HoYoMonad ()
runMove opts = do
  bms <- asks' bookmarks
  let search = moveSearch opts
  case fst $ searchBookmarks search bms of
    []    -> throwError ("Unknown bookmark: " <> show search)
    -- TODO
    [bm]  -> do printStdout ("cd " <> view bookmarkDirectory bm)
                liftIO $ exitWith (ExitFailure 3)
    _     -> throwError "todo"

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' <> s

-- | Run the "list" command: list all the saved bookmarks.
runList :: ListOptions -> HoYoMonad ()
runList opts = do
  let filt = filterBookmarks (listFilterName opts) (listFilterDirectoryInfix opts)
  bms' <- asks' bookmarks
  let bms = filter filt $ sortOn (view bookmarkIndex) $ unBookmarks bms'
  let numberWidth = maximumDefault 1 $ map (length . show . view bookmarkIndex) bms
  displayTime <- asks' (config . displayCreationTime)
  forM_ bms $ \(Bookmark dir idx zTime mbName) -> do
    let num = pad numberWidth (show idx)
    let timeStr = formatTime defaultTimeLocale "%D %T" zTime
    let d = case mbName of Nothing    -> dir
                           Just name  -> dir <> " " <> name
    if displayTime
    then printStdout (num <> ". " <> timeStr <> "\t" <> d)
    else printStdout (num <> ". " <> d)

clearDisabledErrMsg :: String
clearDisabledErrMsg = intercalate "\n" [
  "The 'clear' command is disabled by default."
  , "To enable, set enable_clear = true in the config or pass the --enable-clear flag."
  ]

resetDisabledErrMsg :: String
resetDisabledErrMsg = intercalate "\n" [
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

-- | Run the "delete" command: search for a bookmark and delete it.
runDelete :: DeleteOptions -> HoYoMonad ()
runDelete opts = do
  let search = deleteSearch opts
  modifyBookmarksM $ \bms -> do
    let (searchResults, afterDelete) = searchBookmarks search (Bookmarks bms)
    void $ assertVerbose ("no bookmarks found for search " <> show search)
                            $ return $ not $ null searchResults
    void $ assertVerbose ("multiple bookmarks found for search " <> show search)
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
  forM_ keyVals $ \(k, Toml.AnyValue v) -> do
    let kStr = Toml.prettyKey k
    let vStr = valText v
    printStdout $ T.unpack (kStr <> " = " <> vStr)

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

-- | Run the "config" command: dispatch on the given sub-command.
runConfig :: ConfigCommand -> HoYoMonad ()
runConfig (Print opts) = runConfigPrint opts
runConfig (Reset opts) = runConfigReset opts
runConfig (Set opts) = runConfigSet opts

-- | Run a 'Command' in the hoyo environment.
runCommand :: Command -> HoYoMonad ()
runCommand       (Add opts) = runAdd opts
runCommand      (Move opts) = runMove opts
runCommand      (List opts) = runList opts
runCommand     (Clear opts) = runClear opts
runCommand    (Delete opts) = runDelete opts
runCommand   (Refresh opts) = runRefresh opts
runCommand (ConfigCmd opts) = runConfig opts
