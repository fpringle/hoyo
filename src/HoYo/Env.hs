module HoYo.Env (
  -- HoYo config
  Env (..)
  , bookmarks
  , bookmarksPath
  , settings
  , settingsPath
  , initEnv
  , getEnv
  ) where

import HoYo.Types
import HoYo.Settings
import HoYo.Bookmark

import qualified Data.Text as T
import Data.Bifunctor (first)

import Control.Monad.IO.Class

import Control.Monad (unless)

import Lens.Simple

import System.Directory
import System.FilePath

writeEnv :: MonadIO m => Env -> m ()
writeEnv env = do
  encodeBookmarksFile (view bookmarksPath env) (view bookmarks env)
  encodeSettingsFile (view settingsPath env) (view settings env)

readEnv :: MonadIO m => FilePath -> FilePath -> m (Either String Env)
readEnv bFp sFp = do
  bs <- first T.unpack <$> decodeBookmarksFile bFp
  se <- first T.unpack <$> decodeSettingsFile sFp
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

initEnv :: MonadIO m => FilePath -> FilePath -> m ()
initEnv bFp sFp = do
  initPath sFp
  initPath bFp
  let env = Env defaultBookmarks bFp defaultSettings sFp
  writeEnv env

initBookmarksIfNotExists :: MonadIO m => FilePath -> m ()
initBookmarksIfNotExists fp' = do
  fp <- liftIO $ makeAbsolute fp'
  ex <- liftIO $ doesFileExist fp
  unless ex $ do
    initPath fp
    encodeBookmarksFile fp defaultBookmarks

initSettingsIfNotExists :: MonadIO m => FilePath -> m ()
initSettingsIfNotExists fp' = do
  fp <- liftIO $ makeAbsolute fp'
  ex <- liftIO $ doesFileExist fp
  unless ex $ do
    initPath fp
    encodeSettingsFile fp defaultSettings

initEnvIfNotExists :: MonadIO m => FilePath -> FilePath -> m ()
initEnvIfNotExists bFp sFp = do
  initBookmarksIfNotExists bFp
  initSettingsIfNotExists sFp

getEnv :: MonadIO m => FilePath -> FilePath -> m (Either String Env)
getEnv bFp' sFp' = do
  sFp <- liftIO $ makeAbsolute sFp'
  bFp <- liftIO $ makeAbsolute bFp'
  initEnvIfNotExists bFp sFp
  readEnv bFp sFp
