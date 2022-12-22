module HoYo.Command where

import HoYo.Types
import HoYo.Utils

import Data.List

import Control.Monad.IO.Class
import Control.Monad (forM_)

newtype AddOptions = AddOptions {
  addDirectory :: FilePath
  }

newtype MoveOptions = MoveOptions {
  moveIndex :: Int
  }

data ListOptions = ListOptions {
  }

data Command =
  Add AddOptions
  | Move MoveOptions
  | List ListOptions

newtype GlobalOptions = GlobalOptions {
  configPath    :: FilePath
  }

data Options = Options {
  optCommand    :: Command
  , optGlobals  :: GlobalOptions
  }

runAdd :: AddOptions -> HoYoMonad ()
runAdd opts = liftIO $ putStrLn ("bookmark dir " ++ addDirectory opts)

runMove :: MoveOptions -> HoYoMonad ()
runMove opts = liftIO $ putStrLn ("move to dir bookmark #" ++ show (moveIndex opts))

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' <> s

runList :: ListOptions -> HoYoMonad ()
runList _ = do
  bms <- sort <$> asks' bookmarks
  let numWidth = 4
  forM_ bms $ \(Bookmark dir idx) -> do
    let num = pad numWidth (show idx)
    liftIO $ putStrLn (num <> ". " <> dir)

runCommand :: Command -> HoYoMonad ()
runCommand (Add opts)   = runAdd opts
runCommand (Move opts)  = runMove opts
runCommand (List opts)  = runList opts
