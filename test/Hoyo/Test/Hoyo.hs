module Hoyo.Test.Hoyo where

import           Control.Exception

import qualified Data.Text               as T

import           Hoyo

import           System.Directory
import           System.IO.Temp

import           Test.QuickCheck
import           Test.QuickCheck.Monadic as Q

testHoyoMonadProperty :: HoyoMonad a -> Env -> (Either T.Text a -> Bool) -> Property
testHoyoMonadProperty hoyo env test = monadicIO $ do
  res <- run (runHoyo hoyo env)
  Q.assert (test res)

testHoyoMonadEq :: Eq a => HoyoMonad a -> Env -> Either T.Text a -> Property
testHoyoMonadEq hoyo env expected = testHoyoMonadProperty hoyo env (== expected)

withEnv :: (Env -> IO a) -> Bookmarks -> Config -> IO a
withEnv func bms cfg = bracket createFiles deleteFiles runFunc
  where
  -- create temp files BM and CFG
  -- write bms to BM
  -- write cfg to CFG
    createFiles = do
      let bmsString = T.unpack $ encodeBookmarks bms
      let cfgString = T.unpack $ encodeConfig cfg
      bFile <- writeSystemTempFile "bookarks.toml" bmsString
      cFile <- writeSystemTempFile "config.toml" cfgString
      return (bFile, cFile)

  -- create env
  -- run func
    runFunc (bFile, cFile) = do
      let env = Env bms bFile cfg cFile
      func env

  -- cleanup
    deleteFiles (bFile, cFile) = do
      removeFile bFile
      removeFile cFile

testHoyoMonadPropertyWithEnv :: Bookmarks -> Config -> HoyoMonad a -> (Either T.Text a -> Bool) -> Property
testHoyoMonadPropertyWithEnv bms cfg hoyo test = monadicIO $ do
  result <- run (withEnv (runHoyo hoyo) bms cfg)
  Q.assert (test result)

testHoyoMonadEqWithEnv :: Eq a => Bookmarks -> Config -> HoyoMonad a -> Either T.Text a -> Property
testHoyoMonadEqWithEnv bms cfg hoyo expected = testHoyoMonadPropertyWithEnv bms cfg hoyo (== expected)
