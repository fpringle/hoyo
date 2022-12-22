module HoYo.Command where

import HoYo.Types

import Control.Monad.IO.Class

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

runAdd :: AddOptions -> HoYoMonad ()
runAdd opts = liftIO $ putStrLn ("bookmark dir " ++ addDirectory opts)

runMove :: MoveOptions -> HoYoMonad ()
runMove opts = liftIO $ putStrLn ("move to dir bookmark #" ++ show (moveIndex opts))

runList :: ListOptions -> HoYoMonad ()
runList _ = liftIO $ putStrLn "list bookmarks"

runCommand :: Command -> HoYoMonad ()
runCommand (Add opts)   = runAdd opts
runCommand (Move opts)  = runMove opts
runCommand (List opts)  = runList opts
