-- | The read-only hoyo environment.
module HoYo.Env (
  -- HoYo config
  Env (..)
  -- , bookmarks
  -- , bookmarksPath
  -- , config
  -- , configPath
  , initEnv
  , getEnv
  , writeEnv
  , readEnv
  ) where

import HoYo.Types
import HoYo.Config
import HoYo.Bookmark

import qualified Data.Text as T
import Data.Bifunctor (first)

import Control.Monad.IO.Class

import Control.Monad (unless)

import Lens.Simple

import System.Directory
import System.FilePath

-- | Write an 'Env' to file.
writeEnv :: MonadIO m => Env -> m ()
writeEnv env = do
  encodeBookmarksFile (view bookmarksPath env) (view bookmarks env)
  encodeConfigFile (view configPath env) (view config env)

-- | Read an 'Env' from a file.
readEnv :: MonadIO m => FilePath -> FilePath -> m (Either String Env)
readEnv bFp sFp = do
  bs <- first T.unpack <$> decodeBookmarksFile bFp
  se <- first T.unpack <$> decodeConfigFile sFp
  case (bs, se) of
    (Right b, Right s)  -> return $ Right (Env b bFp s sFp)
    (Left e, Right _)   -> return $ Left e
    (Right _, Left e)   -> return $ Left e
    (Left e1, Left e2)  -> return $ Left (unlines [e1, e2])

initPath :: MonadIO m => FilePath -> m ()
initPath fp' = do
  fp <- liftIO $ makeAbsolute fp'
  let dir = takeDirectory fp
  liftIO $ createDirectoryIfMissing True dir

emptyBookmarks :: Bookmarks
emptyBookmarks = Bookmarks []

-- | Given a filepath for the bookmarks file and a filepath for the config file,
-- initialize the respective TOMLs at those locations.
initEnv :: MonadIO m => FilePath -> FilePath -> m ()
initEnv bFp sFp = do
  initPath sFp
  initPath bFp
  let env = Env emptyBookmarks bFp defaultConfig sFp
  writeEnv env

initBookmarksIfNotExists :: MonadIO m => FilePath -> m ()
initBookmarksIfNotExists fp' = do
  fp <- liftIO $ makeAbsolute fp'
  ex <- liftIO $ doesFileExist fp
  unless ex $ do
    initPath fp
    encodeBookmarksFile fp emptyBookmarks

initConfigIfNotExists :: MonadIO m => FilePath -> m ()
initConfigIfNotExists fp' = do
  fp <- liftIO $ makeAbsolute fp'
  ex <- liftIO $ doesFileExist fp
  unless ex $ do
    initPath fp
    encodeConfigFile fp defaultConfig

initEnvIfNotExists :: MonadIO m => FilePath -> FilePath -> m ()
initEnvIfNotExists bFp sFp = do
  initBookmarksIfNotExists bFp
  initConfigIfNotExists sFp

-- | Retrieve an 'Env' from given bookmark- and config- file locations.
getEnv :: MonadIO m => FilePath -> FilePath -> m (Either String Env)
getEnv bFp' sFp' = do
  sFp <- liftIO $ makeAbsolute sFp'
  bFp <- liftIO $ makeAbsolute bFp'
  initEnvIfNotExists bFp sFp
  readEnv bFp sFp
