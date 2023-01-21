{-|
Module      : HoYo.Env
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

The read-only hoyo environment.
-}
module HoYo.Env (
  -- * HoYo config
  Env (..)
  , initEnv
  , getEnv
  , writeEnv
  , readEnv

  -- ** Default file paths
  , defaultBookmarksPath
  , defaultConfigPath
  ) where

import HoYo.Bookmark
import HoYo.Config
import HoYo.Types

import qualified Data.Text as T

import Control.Monad.Except
-- import Control.Monad.IO.Class
-- import Control.Monad (unless)

import Lens.Micro.Extras

import System.Directory
import System.FilePath

-- | Write an 'Env' to file.
writeEnv :: MonadIO m => Env -> m ()
writeEnv env = do
  encodeBookmarksFile (view bookmarksPath env) (view bookmarks env)
  encodeConfigFile (view configPath env) (view config env)

-- | Read an 'Env' from a file.
readEnv :: MonadIO m => TFilePath -> TFilePath -> m (Either T.Text Env)
readEnv bFp sFp = do
  bs <- decodeBookmarksFile bFp
  se <- decodeConfigFile sFp
  case (bs, se) of
    (Right b, Right s)  -> return $ Right (Env b bFp s sFp)
    (Left e, Right _)   -> return $ Left e
    (Right _, Left e)   -> return $ Left e
    (Left e1, Left e2)  -> return $ Left (T.unlines [e1, e2])

initPath :: MonadIO m => TFilePath -> m ()
initPath fp' = do
  fp <- liftIO $ makeAbsolute $ T.unpack fp'
  let dir = takeDirectory fp
  liftIO $ createDirectoryIfMissing True dir

-- | Given a filepath for the bookmarks file and a filepath for the config file,
-- initialize the respective TOMLs at those locations.
initEnv :: MonadIO m => TFilePath -> TFilePath -> m ()
initEnv bFp sFp = do
  initPath sFp
  initPath bFp
  bms <- bookmarksFromDefault $ view defaultBookmarks defaultConfig
  let env = Env bms bFp defaultConfig sFp
  writeEnv env

initBookmarksIfNotExists :: (MonadIO m, MonadError T.Text m) => Config -> TFilePath -> m Bookmarks
initBookmarksIfNotExists cfg fp' = do
  fp <- liftIO $ makeAbsolute $ T.unpack fp'
  let fpText = T.pack fp
  ex <- liftIO $ doesFileExist fp
  unless ex $ do
    initPath fpText
    bms <- bookmarksFromDefault $ view defaultBookmarks cfg
    encodeBookmarksFile fpText bms
  decodeBookmarksFile fpText >>= liftEither

initConfigIfNotExists :: (MonadIO m, MonadError T.Text m) => TFilePath -> m Config
initConfigIfNotExists fp' = do
  fp <- liftIO $ makeAbsolute $ T.unpack fp'
  let fpText = T.pack fp
  exists <- liftIO $ doesFileExist fp
  unless exists $ do
    initPath fpText
    encodeConfigFile fpText defaultConfig
  decodeConfigFile fpText >>= liftEither

initEnvIfNotExists :: (MonadIO m, MonadError T.Text m) => TFilePath -> TFilePath -> m Env
initEnvIfNotExists bFp sFp = do
  cfg <- initConfigIfNotExists sFp
  bms <- initBookmarksIfNotExists cfg bFp
  return $ Env bms bFp cfg sFp

-- | Retrieve an 'Env' from given bookmark- and config- file locations.
getEnv :: MonadIO m => TFilePath -> TFilePath -> m (Either T.Text Env)
getEnv bFp' sFp' = do
  sFp <- T.pack <$> liftIO (makeAbsolute $ T.unpack sFp')
  bFp <- T.pack <$> liftIO (makeAbsolute $ T.unpack bFp')
  runExceptT $ initEnvIfNotExists bFp sFp

-- | The default path for the hoyo config. Usually $HOME/.config/hoyo/config.toml
defaultConfigPath :: IO TFilePath
defaultConfigPath = T.pack <$> getXdgDirectory XdgConfig "hoyo/config.toml"

-- |The default path for hoyo bookmarks. Usually $HOME/.local/share/hoyo/config.toml
defaultBookmarksPath :: IO TFilePath
defaultBookmarksPath = T.pack <$> getXdgDirectory XdgData "hoyo/bookmarks.toml"
