module GoTo.Settings where

import GoTo.Types

import Toml (TomlCodec, (.=))
import qualified Toml

settingsCodec :: TomlCodec Settings
settingsCodec = pure Settings
