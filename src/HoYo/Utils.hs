{-# LANGUAGE RankNTypes #-}
module HoYo.Utils where

import HoYo.Types

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Reader.Class (MonadReader (ask))

import Lens.Simple

runHoYo :: HoYoMonad a -> Env -> IO (Either String a)
runHoYo = runReaderT . runExceptT . unHoYo

asks' :: MonadReader a m => Getter a a' b b' -> m b
asks' getter = view getter <$> ask

maximumDefault :: Ord a => a -> [a] -> a
maximumDefault def [] = def
maximumDefault _ xs = maximum xs
