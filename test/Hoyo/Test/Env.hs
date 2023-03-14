{-# LANGUAGE TemplateHaskell #-}
module Hoyo.Test.Env where

import qualified Data.Text               as T

import           Hoyo
import           Hoyo.Test.Bookmark
import           Hoyo.Test.Gen           ()
import           Hoyo.Test.Hoyo

import           System.IO.Temp

import           Test.QuickCheck
import           Test.QuickCheck.Monadic as Q


envEq :: Env -> Env -> Bool
envEq (Env bms1 bfp1 cfg1 cfp1) (Env bms2 bfp2 cfg2 cfp2) =
  bookmarksEq bms1 bms2
    && bfp1 == bfp2
    && cfg1 == cfg2
    && cfp1 == cfp2

eitherEnvEq :: Either a Env -> Either a Env -> Bool
eitherEnvEq (Right e1) (Right e2) = envEq e1 e2
eitherEnvEq _ _                   = False

testWriteReadEnv :: Bookmarks -> Config -> Property
testWriteReadEnv bms cfg = monadicIO $ do
  (writtenEnv, readenEnv) <- run $ withEnv writeRead bms cfg
  Q.assert (eitherEnvEq (Right writtenEnv) readenEnv)

  where
    writeRead :: Env -> IO (Env, Either T.Text Env)
    writeRead env = do
      let Env _ bFile _ cFile = env
      env2 <- readEnv bFile cFile
      return (env, env2)

prop_WriteReadEnv :: Property
prop_WriteReadEnv = withMaxSuccess 10 testWriteReadEnv

prop_InitEnv :: Property
prop_InitEnv = ioProperty $ do
  bFile <- emptySystemTempFile "bookmarks.toml"
  cFile <- emptySystemTempFile "config.toml"
  initEnv bFile cFile
  writtenEnv <- readEnv bFile cFile
  let expectedEnv = Env (Bookmarks []) bFile defaultConfig cFile
  return (eitherEnvEq writtenEnv (Right expectedEnv))

return []
envTests :: IO Bool
envTests = $quickCheckAll
