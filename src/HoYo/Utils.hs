{-# LANGUAGE RankNTypes #-}
module HoYo.Utils where

import HoYo.Types

import Control.Monad (when, unless)
import Control.Monad.Except (runExceptT, throwError)
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

assert :: String -> HoYoMonad Bool -> HoYoMonad Bool
assert err check = do
  res <- check
  unless res $ throwError err
  return res

assertVerbose :: String -> HoYoMonad Bool -> HoYoMonad Bool
assertVerbose err check = do
  shouldFail <- asks' (settings . failOnError)
  res <- check
  when (shouldFail && not res) $ throwError err
  return res
