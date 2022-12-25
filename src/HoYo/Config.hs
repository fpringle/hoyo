{-# LANGUAGE RankNTypes #-}
module HoYo.Config where

import HoYo.Types
import HoYo.Utils

import qualified Data.Text as T
import Data.Bifunctor (first)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

import Toml (TomlCodec)
import qualified Toml

import Lens.Simple

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.bool     "fail_on_error"             .== _failOnError
  <*> Toml.bool     "display_creation_time"     .== _displayCreationTime
  <*> Toml.bool     "enable_clearing"           .== _enableClearing
  <*> Toml.bool     "enable_reset"              .== _enableReset
  <*> Toml.bool     "backup_before_clear"       .== _backupBeforeClear

defaultConfig :: Config
defaultConfig = Config {
  _failOnError                = False
  , _displayCreationTime      = False
  , _enableClearing           = False
  , _enableReset              = False
  , _backupBeforeClear        = False
  }

decodeConfig :: T.Text -> Either T.Text Config
decodeConfig = first Toml.prettyTomlDecodeErrors . Toml.decodeExact configCodec

decodeConfigFile :: MonadIO m => FilePath -> m (Either T.Text Config)
decodeConfigFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact configCodec

encodeConfig :: Config -> T.Text
encodeConfig = Toml.encode configCodec

encodeConfigFile :: MonadIO m => FilePath -> Config -> m ()
encodeConfigFile fp = void . Toml.encodeToFile configCodec fp

getKeyVals :: Config -> [(Toml.Key, Toml.AnyValue)]
getKeyVals = tomlToKeyVals . Toml.execTomlCodec configCodec

setConfig :: MonadError String m => String -> String -> Config -> m Config
setConfig   "fail_on_error"           val cfg = flip (set failOnError) cfg
                                                  <$> liftEither (readBool val)
setConfig   "display_creation_time"   val cfg = flip (set displayCreationTime) cfg
                                                  <$> liftEither (readBool val)
setConfig   "enable_clearing"         val cfg = flip (set enableClearing) cfg
                                                  <$> liftEither (readBool val)
setConfig   "enable_reset"            val cfg = flip (set enableReset) cfg
                                                  <$> liftEither (readBool val)
setConfig   "backup_before_clear"     val cfg = flip (set backupBeforeClear) cfg
                                                  <$> liftEither (readBool val)
setConfig key _ _ = throwError ("Invalid key: " <> key)
