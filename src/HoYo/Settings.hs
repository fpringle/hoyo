module HoYo.Settings where

import HoYo.Types

import Toml (TomlCodec)
-- import qualified Toml

settingsCodec :: TomlCodec Settings
settingsCodec = pure Settings

defaultSettings :: Settings
defaultSettings = Settings
