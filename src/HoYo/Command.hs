module HoYo.Command where

import HoYo.Types
import HoYo.Utils
import HoYo.Bookmark
import HoYo.Config

import Data.List
import Data.Function

import Control.Applicative

import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader.Class (ask)

import Lens.Simple

import System.Directory

import Data.Time

import qualified Toml

data AddOptions = AddOptions {
  addDirectory  :: FilePath
  , addName     :: Maybe String
  }

newtype MoveOptions = MoveOptions {
  moveSearch :: BookmarkSearchTerm
  }

data ListOptions = ListOptions {
  listFilterName                :: Maybe String
  , listFilterDirectoryInfix    :: Maybe String
  }

data ClearOptions = ClearOptions {
  }

newtype DeleteOptions = DeleteOptions {
  deleteSearch :: BookmarkSearchTerm
  }

data RefreshOptions = RefreshOptions {
  }

data ConfigPrintOptions = ConfigPrintOptions {
  }

data ConfigResetOptions = ConfigResetOptions {
  }

data ConfigSetOptions = ConfigSetOptions {
  setKey        :: String
  , setValue    :: String
  }

data ConfigCommand =
  Print ConfigPrintOptions
  | Reset ConfigResetOptions
  | Set ConfigSetOptions

data Command =
  Add AddOptions
  | Move MoveOptions
  | List ListOptions
  | Clear ClearOptions
  | Delete DeleteOptions
  | Refresh RefreshOptions
  | ConfigCmd ConfigCommand

data MaybeOverride =
  OverrideFalse
  | OverrideTrue
  | NoOverride
  | Conflict

combOverride :: Bool -> Bool -> MaybeOverride
combOverride False False = NoOverride
combOverride True  False = OverrideTrue
combOverride False True  = OverrideFalse
combOverride True  True  = Conflict

data OverrideOptions = OverrideOptions {
  overrideFailOnError               :: MaybeOverride
  , overrideDisplayCreationTime     :: MaybeOverride
  , overrideEnableClearing          :: MaybeOverride
  , overrideEnableReset             :: MaybeOverride
  }

overrideFunc :: MaybeOverride -> (Bool -> Bool)
overrideFunc NoOverride     = id
overrideFunc OverrideTrue   = const True
overrideFunc OverrideFalse  = const False
overrideFunc Conflict       = error "override conflict!"

overrideConfig :: OverrideOptions -> Config -> Config
overrideConfig opts =
  over failOnError            (overrideFunc $         overrideFailOnError opts)
  . over displayCreationTime  (overrideFunc $ overrideDisplayCreationTime opts)
  . over enableClearing       (overrideFunc $      overrideEnableClearing opts)
  . over enableReset          (overrideFunc $         overrideEnableReset opts)

verifyOverrides :: OverrideOptions -> Maybe String
verifyOverrides (OverrideOptions o1 o2 o3 o4) = verify o1
                                                  <|> verify o2
                                                  <|> verify o3
                                                  <|> verify o4
  where verify Conflict = Just "conflicting flags"
        verify _ = Nothing

data GlobalOptions = GlobalOptions {
  globalConfigPath  :: Maybe FilePath
  , dataPath        :: Maybe FilePath
  , overrides       :: OverrideOptions
  }

data Options = Options {
  optCommand    :: Command
  , optGlobals  :: GlobalOptions
  }

modifyBookmarks :: ([Bookmark] -> [Bookmark]) -> HoYoMonad ()
modifyBookmarks f = modifyBookmarksM (return . f)

modifyBookmarksM :: ([Bookmark] -> HoYoMonad [Bookmark]) -> HoYoMonad ()
modifyBookmarksM f = do
  Env (Bookmarks bms) bFp _ _ <- ask
  newBookmarks <- Bookmarks <$> f bms
  encodeBookmarksFile bFp newBookmarks

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

runMove :: MoveOptions -> HoYoMonad ()
runMove opts = do
  bms <- asks' bookmarks 
  let search = moveSearch opts
  case fst $ searchBookmarks search bms of
    []    -> liftIO $ putStrLn ("Unknown bookmark: " <> show search)
    [bm]  -> liftIO $ putStrLn ("Move to bookmark " <> show search
                                      <> ": " <> view bookmarkDirectory bm)
    _     -> error "todo"

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' <> s

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
    then liftIO $ putStrLn (num <> ". " <> timeStr <> "\t" <> d)
    else liftIO $ putStrLn (num <> ". " <> d)

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

runClear :: ClearOptions -> HoYoMonad ()
runClear _ = do
  assert clearDisabledErrMsg (asks' (config . enableClearing))

  path <- asks' bookmarksPath
  backup <- asks' (config . backupBeforeClear)
  when backup $ backupFile path "bkp"

  modifyBookmarks $ const []

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

runRefresh :: RefreshOptions -> HoYoMonad ()
runRefresh _ = modifyBookmarks $
  zipWith (set bookmarkIndex) [1..]                           -- re-index from 1
    . nubBy ((==) `on` view bookmarkDirectory)                -- remove duplicate directories
    . sortOn (zonedTimeToUTC . view bookmarkCreationTime)     -- sort by creation time

runConfigPrint :: ConfigPrintOptions -> HoYoMonad ()
runConfigPrint _ = do
  s <- asks' config
  let keyVals = getKeyVals s
  forM_ keyVals $ \(k, Toml.AnyValue v) -> do
    let kStr = Toml.prettyKey k
    let vStr = valText v
    liftIO $ putStrLn $ T.unpack (kStr <> " = " <> vStr)

runConfigReset :: ConfigResetOptions -> HoYoMonad ()
runConfigReset _ = do
  assert resetDisabledErrMsg (asks' (config . enableReset))

  path <- asks' configPath

  backup <- asks' (config . backupBeforeClear)
  when backup $ backupFile path "bkp"

  encodeConfigFile path defaultConfig

runConfigSet :: ConfigSetOptions -> HoYoMonad ()
runConfigSet opts = do
  let key = setKey opts
  let val = setValue opts
  cfgPath <- asks' configPath
  asks' config
    >>= setConfig key val
    >>= encodeConfigFile cfgPath

runConfig :: ConfigCommand -> HoYoMonad ()
runConfig (Print opts) = runConfigPrint opts
runConfig (Reset opts) = runConfigReset opts
runConfig (Set opts) = runConfigSet opts

runCommand :: Command -> HoYoMonad ()
runCommand       (Add opts) = runAdd opts
runCommand      (Move opts) = runMove opts
runCommand      (List opts) = runList opts
runCommand     (Clear opts) = runClear opts
runCommand    (Delete opts) = runDelete opts
runCommand   (Refresh opts) = runRefresh opts
runCommand (ConfigCmd opts) = runConfig opts
