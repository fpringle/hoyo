{-|
Module      : Hoyo.Config
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Configuration for the hoyo program. This is stored on-disk as a TOML file,
usually at ~/.config/hoyo/config.toml
-}

{-# LANGUAGE RankNTypes #-}

module Hoyo.Config (
  Config (..)

  , defaultConfig
  , decodeConfig
  , decodeConfigFile
  , encodeConfig
  , encodeConfigFile

  , setConfig
  , getKeyVals
  ) where

import                          Control.Category     ((<<<))
import                          Control.Monad
import                          Control.Monad.Catch
import                          Control.Monad.Except

import                          Data.Bifunctor       (first)
import                          Data.Maybe           (maybeToList)
import                qualified Data.Text            as T

import                          Hoyo.Bookmark
import {-# SOURCE #-}           Hoyo.CLI.Parse
import                          Hoyo.Internal.Types
import                          Hoyo.Utils

import                          Lens.Micro
import                          Lens.Micro.Extras

import                          Options.Applicative

import                qualified Toml
import                          Toml                 (TomlCodec, (.=))

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
  <*> (MaybeV . fmap CommandV <$> Toml.dioptional (commandCodec "default_command")) .= view defaultCommand

-- | Toml codec using optparse-applicative to parse a default command. Incurs a cyclic
-- dependency which is resolved using Parse.hs-boot.
commandCodec :: Toml.Key -> TomlCodec Command
commandCodec = Toml.match (Toml._Text <<< Toml.invert (Toml.prism fmt prs))
  where
    fmt :: Command -> T.Text
    fmt = formatArgs . formatCommand

    prs :: T.Text -> Either Toml.TomlBiMapError Command
    prs t = case execParserPure defaultPrefs (info parseCommand mempty) (splitArgs t) of
      Success cmd -> Right cmd
      Failure err -> do let (msg, _) = renderFailure err "hoyo"
                        Left (Toml.ArbitraryError $ T.pack msg)
      CompletionInvoked res -> Left (Toml.ArbitraryError $ tshow res)

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
decodeConfig :: T.Text -> Either HoyoException Config
decodeConfig = first (ParseException . pure . Toml.prettyTomlDecodeErrors)
                . Toml.decodeExact configCodec

-- | Decode a 'Config' from a file.
decodeConfigFile :: (MonadIO m, MonadCatch m) => FilePath -> m (Either HoyoException Config)
decodeConfigFile = handle catchIOException
                      . fmap (first (ParseException . pure . Toml.prettyTomlDecodeErrors))
                      . Toml.decodeFileExact configCodec

-- | Encode a 'Config' to a Text.
encodeConfig :: Config -> T.Text
encodeConfig = Toml.encode configCodec

-- | Encode a 'Config' to a file.
encodeConfigFile :: (MonadIO m, MonadCatch m) => FilePath -> Config -> m (Either HoyoException ())
encodeConfigFile fp = handle catchIOException . fmap Right . void . Toml.encodeToFile configCodec fp

-- | Get TOML key-value pairs from a 'Config'.
getKeyVals :: Config -> [(T.Text, AnyConfigValue)]
getKeyVals cfg = [
    ("fail_on_error",         AnyConfigValue $ _failOnError cfg)
  , ("display_creation_time", AnyConfigValue $ _displayCreationTime cfg)
  , ("enable_clearing",       AnyConfigValue $ _enableClearing cfg)
  , ("enable_reset",          AnyConfigValue $ _enableReset cfg)
  , ("backup_before_clear",   AnyConfigValue $ _backupBeforeClear cfg)
    ] <> maybeToList (("default_command", ) . AnyConfigValue <$> view (__defaultCommand . cfgMaybe) cfg)
      <> [("default_bookmarks", AnyConfigValue $ _defaultBookmarks cfg)]

-- | Try to set a key-value pair in the config.
setConfig :: MonadError HoyoException m => T.Text -> T.Text -> Config -> m Config
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
setConfig key _ _ = throwError $ ConfigException ["Invalid key: " <> key]
