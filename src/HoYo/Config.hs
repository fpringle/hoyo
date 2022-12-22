module HoYo.Config (
  -- HoYo config
  Config (..)
  , bookmarks
  , settings

  -- ** Encoding and decoding
  , decodeConfig
  , decodeConfigFile
  , encodeConfig
  , encodeConfigFile
  ) where

import HoYo.Types
import HoYo.Settings
import HoYo.Bookmark

import qualified Data.Text as T
import Data.Bifunctor (first)

import Toml (TomlCodec, (.=))
import qualified Toml

import Control.Monad.IO.Class

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.list bookmarkCodec   "directory"   .= _bookmarks
  <*> Toml.table settingsCodec  "settings"    .= _settings

decodeConfig :: T.Text -> Either T.Text Config
decodeConfig = first Toml.prettyTomlDecodeErrors . Toml.decodeExact configCodec

decodeConfigFile :: MonadIO m => FilePath -> m (Either T.Text Config)
decodeConfigFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact configCodec

encodeConfig :: Config -> T.Text
encodeConfig = Toml.encode configCodec

encodeConfigFile :: MonadIO m => FilePath -> Config -> m T.Text
encodeConfigFile = Toml.encodeToFile configCodec
