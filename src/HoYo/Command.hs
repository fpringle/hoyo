module HoYo.Command where

import HoYo.Types
import HoYo.Utils
import HoYo.Bookmark
import HoYo.Settings

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

newtype AddOptions = AddOptions {
  addDirectory :: FilePath
  }

newtype MoveOptions = MoveOptions {
  moveIndex :: Int
  }

data ListOptions = ListOptions {
  }

data ClearOptions = ClearOptions {
  }

newtype DeleteOptions = DeleteOptions {
  deleteIndex :: Int
  }

data RefreshOptions = RefreshOptions {
  }

data ConfigPrintOptions = ConfigPrintOptions {
  }

data ConfigResetOptions = ConfigResetOptions {
  }

data ConfigCommand =
  Print ConfigPrintOptions
  | Reset ConfigResetOptions

data Command =
  Add AddOptions
  | Move MoveOptions
  | List ListOptions
  | Clear ClearOptions
  | Delete DeleteOptions
  | Refresh RefreshOptions
  | Config ConfigCommand

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

overrideSettings :: OverrideOptions -> Settings -> Settings
overrideSettings opts =
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
  configPath    :: Maybe FilePath
  , dataPath    :: Maybe FilePath
  , overrides   :: OverrideOptions
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
  dir <- liftIO $ makeAbsolute (addDirectory opts)
  void $ assertVerbose "not a directory" $ liftIO $ doesDirectoryExist dir
  modifyBookmarksM $ \bms -> do
    uniq <- assertVerbose "bookmark already exists" $
      return $ all ((/= dir) . view bookmarkDirectory) bms
    if uniq
    then do
      let maxIndex = maximumDefault 0 $ map (view bookmarkIndex) bms
      zTime <- liftIO getZonedTime
      let newBookMark = Bookmark dir (maxIndex + 1) zTime
      return (newBookMark : bms)
    else return bms

runMove :: MoveOptions -> HoYoMonad ()
runMove opts = do
  bms <- asks' bookmarks 
  let idx = moveIndex opts
  case lookupBookmark idx bms of
    Nothing -> liftIO $ putStrLn ("Unknown bookmark: #" <> show idx)
    Just bm  -> liftIO $ putStrLn ("move to dir bookmark #" <> show idx
                                      <> ": " <> view bookmarkDirectory bm)

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' <> s

runList :: ListOptions -> HoYoMonad ()
runList _ = do
  bms <- sortOn (view bookmarkIndex) . unBookmarks <$> asks' bookmarks
  let numberWidth = maximumDefault 1 $ map (length . show . view bookmarkIndex) bms
  displayTime <- asks' (settings . displayCreationTime)
  forM_ bms $ \(Bookmark dir idx zTime) -> do
    let num = pad numberWidth (show idx)
    let timeStr = formatTime defaultTimeLocale "%D %T" zTime
    if displayTime
    then liftIO $ putStrLn (num <> ". " <> timeStr <> "\t" <> dir)
    else liftIO $ putStrLn (num <> ". " <> dir)

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
  assert clearDisabledErrMsg (asks' (settings . enableClearing))
  modifyBookmarks $ const []

runDelete :: DeleteOptions -> HoYoMonad ()
runDelete opts = do
  let idx = deleteIndex opts
  modifyBookmarksM $ \bms -> do
    let sameIdx = filter ((== idx) . view bookmarkIndex) bms
    void $ assertVerbose ("no bookmark with index #" <> show idx) $ return $ not $ null sameIdx
    void $ assertVerbose ("multiple bookmark with index #" <> show idx) $ return $ length sameIdx == 1
    return $ filter ((/= idx) . view bookmarkIndex) bms

runRefresh :: RefreshOptions -> HoYoMonad ()
runRefresh _ = modifyBookmarks $
  zipWith (set bookmarkIndex) [1..]                           -- re-index from 1
    . nubBy ((==) `on` view bookmarkDirectory)                -- remove duplicate directories
    . sortOn (zonedTimeToUTC . view bookmarkCreationTime)     -- sort by creation time

runConfigPrint :: ConfigPrintOptions -> HoYoMonad ()
runConfigPrint _ = do
  s <- asks' settings
  let keyVals = getKeyVals s
  forM_ keyVals $ \(k, Toml.AnyValue v) -> do
    let kStr = Toml.prettyKey k
    let vStr = valText v
    liftIO $ putStrLn $ T.unpack (kStr <> " = " <> vStr)

runConfigReset :: ConfigResetOptions -> HoYoMonad ()
runConfigReset _ = do
  assert resetDisabledErrMsg (asks' (settings . enableReset))
  cfgPath <- asks' settingsPath
  encodeSettingsFile cfgPath defaultSettings

runConfig :: ConfigCommand -> HoYoMonad ()
runConfig (Print opts) = runConfigPrint opts
runConfig (Reset opts) = runConfigReset opts

runCommand :: Command -> HoYoMonad ()
runCommand (Add opts)   = runAdd opts
runCommand (Move opts)  = runMove opts
runCommand (List opts)  = runList opts
runCommand (Clear opts)  = runClear opts
runCommand (Delete opts)  = runDelete opts
runCommand (Refresh opts)  = runRefresh opts
runCommand (Config opts)   = runConfig opts
