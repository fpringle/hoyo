module HoYo.Config where

import HoYo.Types
import HoYo.Utils

import qualified Data.Text as T
import Data.Bifunctor (first)

import Control.Monad
import Control.Monad.IO.Class

import Toml (TomlCodec)
import qualified Toml

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.bool     "fail_on_error"             .== _failOnError
  <*> Toml.bool     "display_creation_time"     .== _displayCreationTime
  <*> Toml.bool     "enable_clearing"           .== _enableClearing
  <*> Toml.bool     "enable_reset"              .== _enableReset

defaultConfig :: Config
defaultConfig = Config {
  _failOnError                = False
  , _displayCreationTime      = False
  , _enableClearing           = False
  , _enableReset              = False
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
