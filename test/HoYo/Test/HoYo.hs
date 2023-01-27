module HoYo.Test.HoYo where

import HoYo

import Control.Exception
import qualified Data.Text as T
import System.Directory
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic as Q

testHoYoMonadProperty :: HoYoMonad a -> Env -> (Either T.Text a -> Bool) -> Property
testHoYoMonadProperty hoyo env test = monadicIO $ do
  res <- run (runHoYo hoyo env)
  Q.assert (test res)

testHoYoMonadEq :: Eq a => HoYoMonad a -> Env -> Either T.Text a -> Property
testHoYoMonadEq hoyo env expected = testHoYoMonadProperty hoyo env (== expected)

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
      let env = Env bms (T.pack bFile) cfg (T.pack cFile)
      func env

  -- cleanup
    deleteFiles (bFile, cFile) = do
      removeFile bFile
      removeFile cFile

testHoYoMonadPropertyWithEnv :: Bookmarks -> Config -> HoYoMonad a -> (Either T.Text a -> Bool) -> Property
testHoYoMonadPropertyWithEnv bms cfg hoyo test = monadicIO $ do
  result <- run (withEnv (runHoYo hoyo) bms cfg)
  Q.assert (test result)

testHoYoMonadEqWithEnv :: Eq a => Bookmarks -> Config -> HoYoMonad a -> Either T.Text a -> Property
testHoYoMonadEqWithEnv bms cfg hoyo expected = testHoYoMonadPropertyWithEnv bms cfg hoyo (== expected)
