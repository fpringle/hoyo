module HoYo.Settings where

import HoYo.Types

import qualified Data.Text as T
import Data.Bifunctor (first)

import Control.Monad
import Control.Monad.IO.Class

import Toml (TomlCodec)
import qualified Toml

settingsCodec :: TomlCodec Settings
settingsCodec = pure Settings

defaultSettings :: Settings
defaultSettings = Settings

decodeSettings :: T.Text -> Either T.Text Settings
decodeSettings = first Toml.prettyTomlDecodeErrors . Toml.decodeExact settingsCodec

decodeSettingsFile :: MonadIO m => FilePath -> m (Either T.Text Settings)
decodeSettingsFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact settingsCodec

encodeSettings :: Settings -> T.Text
encodeSettings = Toml.encode settingsCodec

encodeSettingsFile :: MonadIO m => FilePath -> Settings -> m ()
encodeSettingsFile fp = void . Toml.encodeToFile settingsCodec fp
