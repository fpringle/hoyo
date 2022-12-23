module HoYo.Settings where

import HoYo.Types
import HoYo.Utils

import qualified Data.Text as T
import Data.Bifunctor (first)

import Control.Monad
import Control.Monad.IO.Class

import Toml (TomlCodec)
import qualified Toml

settingsCodec :: TomlCodec Settings
settingsCodec = Settings
  <$> Toml.bool     "fail_on_error"             .== _failOnError
  <*> Toml.bool     "display_creation_time"     .== _displayCreationTime
  <*> Toml.bool     "enable_clearing"           .== _enableClearing

defaultSettings :: Settings
defaultSettings = Settings {
  _failOnError                = False
  , _displayCreationTime      = False
  , _enableClearing           = False
  }

decodeSettings :: T.Text -> Either T.Text Settings
decodeSettings = first Toml.prettyTomlDecodeErrors . Toml.decodeExact settingsCodec

decodeSettingsFile :: MonadIO m => FilePath -> m (Either T.Text Settings)
decodeSettingsFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact settingsCodec

encodeSettings :: Settings -> T.Text
encodeSettings = Toml.encode settingsCodec

encodeSettingsFile :: MonadIO m => FilePath -> Settings -> m ()
encodeSettingsFile fp = void . Toml.encodeToFile settingsCodec fp

getKeyVals :: Settings -> [(Toml.Key, Toml.AnyValue)]
getKeyVals = tomlToKeyVals . Toml.execTomlCodec settingsCodec
