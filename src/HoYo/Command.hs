module HoYo.Command where

import HoYo.Types

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
runAdd = error "todo"

runMove :: MoveOptions -> HoYoMonad ()
runMove = error "todo"

runList :: ListOptions -> HoYoMonad ()
runList = error "todo"

runCommand :: Command -> HoYoMonad ()
runCommand (Add opts)   = runAdd opts
runCommand (Move opts)  = runMove opts
runCommand (List opts)  = runList opts
