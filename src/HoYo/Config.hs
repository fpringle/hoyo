{-# LANGUAGE RankNTypes #-}
-- | Configuration for the hoyo program. This is stored on-disk as a TOML file,
-- usually at ~/.config/hoyo/config.toml
module HoYo.Config (
  Config (..)
  -- , failOnError
  -- , displayCreationTime
  -- , enableClearing
  -- , enableReset
  -- , backupBeforeClear

  , defaultConfig
  , decodeConfig
  , decodeConfigFile
  , encodeConfig
  , encodeConfigFile

  , setConfig
  , getKeyVals
  ) where

import HoYo.Types
import HoYo.Utils

import Data.Bifunctor (first)
import qualified Data.Text as T

import Control.Monad
import Control.Monad.Except

import qualified Toml
import Toml (TomlCodec)

import Lens.Simple

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.bool     "fail_on_error"             .== _failOnError
  <*> Toml.bool     "display_creation_time"     .== _displayCreationTime
  <*> Toml.bool     "enable_clearing"           .== _enableClearing
  <*> Toml.bool     "enable_reset"              .== _enableReset
  <*> Toml.bool     "backup_before_clear"       .== _backupBeforeClear

-- | The default config for hoyo.
defaultConfig :: Config
defaultConfig = Config {
  _failOnError                = False
  , _displayCreationTime      = False
  , _enableClearing           = False
  , _enableReset              = False
  , _backupBeforeClear        = False
  }

-- | Decode a 'Config' from a Text.
decodeConfig :: T.Text -> Either T.Text Config
decodeConfig = first Toml.prettyTomlDecodeErrors . Toml.decodeExact configCodec

-- | Decode a 'Config' from a file.
decodeConfigFile :: MonadIO m => FilePath -> m (Either T.Text Config)
decodeConfigFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact configCodec

-- | Encode a 'Config' to a Text.
encodeConfig :: Config -> T.Text
encodeConfig = Toml.encode configCodec

-- | Encode a 'Config' to a file.
encodeConfigFile :: MonadIO m => FilePath -> Config -> m ()
encodeConfigFile fp = void . Toml.encodeToFile configCodec fp

-- | Get TOML key-value pairs from a 'Config'.
getKeyVals :: Config -> [(Toml.Key, Toml.AnyValue)]
getKeyVals = tomlToKeyVals . Toml.execTomlCodec configCodec

-- | Try to set a key-value pair in the config.
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
