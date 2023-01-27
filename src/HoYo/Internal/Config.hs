{-|
Module      : HoYo.Internal.Config
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3
Maintainer  : freddyjepringle@gmail.com

Internals used by the HoYo.Config module.
-}

{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module HoYo.Internal.Config where

import HoYo.Bookmark
import HoYo.Internal.Types
import HoYo.Internal.Utils

import Data.Bifunctor (first)
import qualified Data.Text as T

import Control.Monad
import Control.Monad.Except

import qualified Toml
import Toml (TomlCodec, (.=))

import Lens.Micro
import Lens.Micro.Extras

import Data.Maybe (maybeToList)

-- | A TOML codec describing how to convert a 'Config' to and from its
-- TOML representation.
configCodec :: TomlCodec Config
configCodec = Config
  <$> (BoolV <$> Toml.bool "fail_on_error")           .= view failOnError
  <*> (BoolV <$> Toml.bool "display_creation_time")   .= view displayCreationTime
  <*> (BoolV <$> Toml.bool "enable_clearing")         .= view enableClearing
  <*> (BoolV <$> Toml.bool "enable_reset")            .= view enableReset
  <*> (BoolV <$> Toml.bool "backup_before_clear")     .= view backupBeforeClear
  <*> (ListOfV . fmap DefaultBookmarkV <$> Toml.list defaultBookmarkCodec "default_bookmark") .= view defaultBookmarks
  <*> (MaybeV . fmap CommandV <$> Toml.dioptional (Toml.text "default_command")) .= view defaultCommand

-- | The default config for hoyo.
defaultConfig :: Config
defaultConfig = Config {
  _failOnError                = BoolV False
  , _displayCreationTime      = BoolV False
  , _enableClearing           = BoolV False
  , _enableReset              = BoolV False
  , _backupBeforeClear        = BoolV False
  , _defaultBookmarks         = ListOfV []
  , _defaultCommand           = MaybeV Nothing
  }

-- | Decode a 'Config' from a Text.
decodeConfig :: T.Text -> Either T.Text Config
decodeConfig = first Toml.prettyTomlDecodeErrors . Toml.decodeExact configCodec

-- | Decode a 'Config' from a file.
decodeConfigFile :: MonadIO m => TFilePath -> m (Either T.Text Config)
decodeConfigFile = fmap (first Toml.prettyTomlDecodeErrors) . Toml.decodeFileExact configCodec . T.unpack

-- | Encode a 'Config' to a Text.
encodeConfig :: Config -> T.Text
encodeConfig = Toml.encode configCodec

-- | Encode a 'Config' to a file.
encodeConfigFile :: MonadIO m => TFilePath -> Config -> m ()
encodeConfigFile fp = void . Toml.encodeToFile configCodec (T.unpack fp)

-- | Get TOML key-value pairs from a 'Config'.
getKeyVals :: Config -> [(T.Text, AnyConfigValue)]
getKeyVals cfg = [
    ("fail_on_error",         AnyConfigValue $ _failOnError cfg)
  , ("display_creation_time", AnyConfigValue $ _displayCreationTime cfg)
  , ("enable_clearing",       AnyConfigValue $ _enableClearing cfg)
  , ("enable_reset",          AnyConfigValue $ _enableReset cfg)
  , ("backup_before_clear",   AnyConfigValue $ _backupBeforeClear cfg)
    ] <> maybeToList (fmap (("default_command", ) . AnyConfigValue) $ getMaybe $ _defaultCommand cfg)
      <> [("default_bookmarks", AnyConfigValue $ _defaultBookmarks cfg)]

-- | Try to set a key-value pair in the config.
setConfig :: MonadError T.Text m => T.Text -> T.Text -> Config -> m Config
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
