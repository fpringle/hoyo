-- | The read-only hoyo environment.
module HoYo.Env (
  -- HoYo config
  Env (..)
  , initEnv
  , getEnv
  , writeEnv
  , readEnv
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
readEnv :: MonadIO m => FilePath -> FilePath -> m (Either T.Text Env)
readEnv bFp sFp = do
  bs <- decodeBookmarksFile bFp
  se <- decodeConfigFile sFp
  case (bs, se) of
    (Right b, Right s)  -> return $ Right (Env b bFp s sFp)
    (Left e, Right _)   -> return $ Left e
    (Right _, Left e)   -> return $ Left e
    (Left e1, Left e2)  -> return $ Left (T.unlines [e1, e2])

initPath :: MonadIO m => FilePath -> m ()
initPath fp' = do
  fp <- liftIO $ makeAbsolute fp'
  let dir = takeDirectory fp
  liftIO $ createDirectoryIfMissing True dir

-- | Given a filepath for the bookmarks file and a filepath for the config file,
-- initialize the respective TOMLs at those locations.
initEnv :: MonadIO m => FilePath -> FilePath -> m ()
initEnv bFp sFp = do
  initPath sFp
  initPath bFp
  bms <- bookmarksFromDefault $ view defaultBookmarks defaultConfig
  let env = Env bms bFp defaultConfig sFp
  writeEnv env

initBookmarksIfNotExists :: (MonadIO m, MonadError T.Text m) => Config -> FilePath -> m Bookmarks
initBookmarksIfNotExists cfg fp' = do
  fp <- liftIO $ makeAbsolute fp'
  ex <- liftIO $ doesFileExist fp
  unless ex $ do
    initPath fp
    bms <- bookmarksFromDefault $ view defaultBookmarks cfg
    encodeBookmarksFile fp bms
  decodeBookmarksFile fp >>= liftEither

initConfigIfNotExists :: (MonadIO m, MonadError T.Text m) => FilePath -> m Config
initConfigIfNotExists fp' = do
  fp <- liftIO $ makeAbsolute fp'
  exists <- liftIO $ doesFileExist fp
  unless exists $ do
    initPath fp
    encodeConfigFile fp defaultConfig
  decodeConfigFile fp >>= liftEither

initEnvIfNotExists :: (MonadIO m, MonadError T.Text m) => FilePath -> FilePath -> m Env
initEnvIfNotExists bFp sFp = do
  cfg <- initConfigIfNotExists sFp
  bms <- initBookmarksIfNotExists cfg bFp
  return $ Env bms bFp cfg sFp

-- | Retrieve an 'Env' from given bookmark- and config- file locations.
getEnv :: MonadIO m => FilePath -> FilePath -> m (Either T.Text Env)
getEnv bFp' sFp' = do
  sFp <- liftIO $ makeAbsolute sFp'
  bFp <- liftIO $ makeAbsolute bFp'
  runExceptT $ initEnvIfNotExists bFp sFp
