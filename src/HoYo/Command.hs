module HoYo.Command where

import HoYo.Types
import HoYo.Utils
import HoYo.Bookmark

import Data.List

import Control.Monad.IO.Class
import Control.Monad (forM_)

import Control.Monad.Reader.Class (ask)

import Lens.Simple

import System.Directory

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

data Command =
  Add AddOptions
  | Move MoveOptions
  | List ListOptions
  | Clear ClearOptions
  | Delete DeleteOptions
  | Refresh RefreshOptions

data GlobalOptions = GlobalOptions {
  configPath    :: Maybe FilePath
  , dataPath    :: Maybe FilePath
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
  modifyBookmarks $ \bms ->
    let maxIndex = maximumDefault 0 $ map (view bookmarkIndex) bms
        newBookMark = Bookmark dir (maxIndex + 1)
    in newBookMark : bms

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
  forM_ bms $ \(Bookmark dir idx) -> do
    let num = pad numberWidth (show idx)
    liftIO $ putStrLn (num <> ". " <> dir)

runClear :: ClearOptions -> HoYoMonad ()
runClear _ = modifyBookmarks $ const []

runDelete :: DeleteOptions -> HoYoMonad ()
runDelete opts = modifyBookmarks $ filter ((/= idx) . view bookmarkIndex)
  where idx = deleteIndex opts

runRefresh :: RefreshOptions -> HoYoMonad ()
runRefresh _ = modifyBookmarks $ zipWith (set bookmarkIndex) [1..] . sortOn (view bookmarkIndex)

runCommand :: Command -> HoYoMonad ()
runCommand (Add opts)   = runAdd opts
runCommand (Move opts)  = runMove opts
runCommand (List opts)  = runList opts
runCommand (Clear opts)  = runClear opts
runCommand (Delete opts)  = runDelete opts
runCommand (Refresh opts)  = runRefresh opts
