{-|
Module      : Hoyo.Internal.Types
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Types used by all the main Hoyo.* modules.
-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK prune #-}

module Hoyo.Internal.Types where

import           Control.Monad.Except       (ExceptT, MonadError (..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Function
import           Data.List                  (intercalate)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import           Data.Time

import           GHC.Generics

import           Language.Haskell.TH.Syntax

import           Lens.Micro
import           Lens.Micro.TH

import           System.IO.Error

-- | The main hoyo read-only environment. Contains the current saved bookmarks,
-- the current hoyo configuration, and the file locations for each.
data Env
  = Env { _bookmarks     :: !Bookmarks
        , _bookmarksPath :: !FilePath
        , _config        :: !Config
        , _configPath    :: !FilePath
        }
  deriving (Show)

-- | Bookmark a directory for easy @cd@. A bookmark remembers the directory,
-- the index, the creation time, and optionally a user-specified nickname
-- for the bookmark.
data Bookmark
  = Bookmark { _bookmarkDirectory    :: !FilePath
             , _bookmarkIndex        :: !Int
             , _bookmarkCreationTime :: !ZonedTime
             , _bookmarkName         :: !(Maybe T.Text)
             }
  deriving (Show)

instance Eq Bookmark where
  Bookmark d1 i1 _ n1 == Bookmark d2 i2 _ n2
    = d1 == d2
   && i1 == i2
   && n1 == n2

-- | Default bookmarks to save at init. A default bookmark remembers the directory
-- and optionally a user-specified nickname for the bookmark.
data DefaultBookmark
  = DefaultBookmark { _defaultBookmarkDirectory :: !FilePath
                    , _defaultBookmarkName      :: !(Maybe T.Text)
                    }
  deriving (Show, Eq)

-- | Wrapper for @['Bookmark']@.
newtype Bookmarks
  = Bookmarks { unBookmarks :: [Bookmark] }
  deriving (Show)

-- | Data-type for represting a bookmark search. You can either search
-- by index or by name. Used by the @delete@ and @move@ commands.
data BookmarkSearchTerm
  = SearchIndex Int
  | SearchName T.Text
  deriving (Show, Eq)

-- | The types of config values allowed in the Hoyo config.
data ConfigValueType
  = TBool
  | TDefaultBookmark
  | TCommand
  | TList ConfigValueType
  | TMaybe ConfigValueType

-- | Values in the Hoyo config. Using a GADT parameterised by 'ConfigValueType'
-- gives us stricter type safety.
data ConfigValue (t :: ConfigValueType) where
  BoolV :: Bool -> ConfigValue 'TBool
  DefaultBookmarkV :: DefaultBookmark -> ConfigValue 'TDefaultBookmark
  CommandV :: Command -> ConfigValue 'TCommand
  ListOfV :: forall (a :: ConfigValueType). [ConfigValue a] -> ConfigValue ('TList a)
  MaybeV :: forall (a :: ConfigValueType). Maybe (ConfigValue a) -> ConfigValue ('TMaybe a)

instance Show (ConfigValue (t :: ConfigValueType)) where
  show (BoolV bool)          = show bool
  show (DefaultBookmarkV bm) = show bm
  show (CommandV t)          = show t
  show (ListOfV xs)          = "[" <> intercalate ", " (map show xs) <> "]"
  show (MaybeV xs)           = show xs

instance Eq (ConfigValue (t :: ConfigValueType)) where
  BoolV b1            == BoolV b2            = b1 == b2
  DefaultBookmarkV b1 == DefaultBookmarkV b2 = b1 == b2
  CommandV b1         == CommandV b2         = b1 == b2
  ListOfV b1          == ListOfV b2          = b1 == b2
  MaybeV b1           == MaybeV b2           = b1 == b2

-- | Existential wrapper around 'ConfigValue'.
data AnyConfigValue
  = forall (t :: ConfigValueType). AnyConfigValue (ConfigValue t)

-- | A representation of hoyo settings.
data Config
  = Config { _failOnError         :: !(ConfigValue 'TBool)
           , _displayCreationTime :: !(ConfigValue 'TBool)
           , _enableClearing      :: !(ConfigValue 'TBool)
           , _enableReset         :: !(ConfigValue 'TBool)
           , _backupBeforeClear   :: !(ConfigValue 'TBool)
           , _defaultBookmarks    :: !(ConfigValue ('TList 'TDefaultBookmark))
           , _defaultCommand      :: !(ConfigValue ('TMaybe 'TCommand))
           }
  deriving (Show, Eq)

-- | Report an exception while searching for a bookmark.
data SearchException
  = NothingFound BookmarkSearchTerm
  | TooManyResults BookmarkSearchTerm [T.Text]
  deriving (Show, Eq, Generic)

-- | Report an exception while running a command.
data CommandException
  = SearchException SearchException
  | InvalidArgumentException [T.Text]
  | LoopException
  deriving (Show, Eq, Generic)

-- | Report a file system exception.
data FileSystemException
  = NoFileException FilePath
  | NoDirException FilePath
  deriving (Show, Eq, Generic)

-- | A custom hierarchical exception type for hoyo.
data HoyoException
  = ConfigException [T.Text]
  | CommandException CommandException
  | IOException IOError
  | FileSystemException FileSystemException
  | ParseException [T.Text]
  | MultipleExceptions (NE.NonEmpty HoyoException)
  deriving (Show, Eq, Generic)

instance Semigroup HoyoException where
  MultipleExceptions ne1 <> MultipleExceptions ne2 = MultipleExceptions (ne1 <> ne2)
  MultipleExceptions ne <> x = MultipleExceptions (ne <> (x NE.:| []))
  x <> MultipleExceptions ne = MultipleExceptions (x NE.<| ne)
  ConfigException xs <> ConfigException ys = ConfigException $ xs <> ys
  ParseException xs <> ParseException ys = ParseException $ xs <> ys
  CommandException (InvalidArgumentException xs) <> CommandException (InvalidArgumentException ys)
    = CommandException (InvalidArgumentException (xs <> ys))
  x <> y = MultipleExceptions (x NE.:| [y])

-- | 'HoyoMonad' is the main monad stack for the hoyo program. It's essentially a wrapper
-- around @ExceptT T.Text (ReaderT Env IO)@: in other words,
-- @HoyoMonad a@ is equivalent to @Env -> IO (Either T.Text a)@
newtype HoyoMonad a
  = HoyoMonad { unHoyo :: ExceptT HoyoException (ReaderT Env IO) a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError HoyoException
  , MonadReader Env
  )

instance MonadIO HoyoMonad where
  liftIO m = HoyoMonad (liftIO $ tryIOError m) >>= \case
    Left err     -> throwError $ IOException err
    Right result -> return result

-- | Options for the "add" command to be parsed from the command-line.
data AddOptions
  = AddOptions { addDirectory :: FilePath
               , addName      :: Maybe T.Text
               }
  deriving (Show, Eq)

-- | Options for the "move" command to be parsed from the command-line.
newtype MoveOptions
  = MoveOptions { moveSearch :: BookmarkSearchTerm }
  deriving (Show, Eq)

-- | Options for the "list" command to be parsed from the command-line.
data ListOptions
  = ListOptions { listFilterName           :: Maybe T.Text
                , listFilterDirectoryInfix :: Maybe T.Text
                , listJSONOutput           :: Bool
                }
  deriving (Show, Eq)

-- | Options for the "clear" command to be parsed from the command-line.
data ClearOptions = ClearOptions deriving (Show, Eq)

-- | Options for the "delete" command to be parsed from the command-line.
newtype DeleteOptions
  = DeleteOptions { deleteSearch :: BookmarkSearchTerm }
  deriving (Show, Eq)

-- | Options for the "refresh" command to be parsed from the command-line.
data RefreshOptions = RefreshOptions deriving (Show, Eq)

-- | Options for the "config print" command to be parsed from the command-line.
newtype ConfigPrintOptions
  = ConfigPrintOptions { configPrintJSONOuput :: Bool }
  deriving (Show, Eq)

-- | Options for the "config reset" command to be parsed from the command-line.
data ConfigResetOptions = ConfigResetOptions deriving (Show, Eq)

-- | Options for the "config set" command to be parsed from the command-line.
data ConfigSetOptions
  = ConfigSetOptions { setKey   :: T.Text
                     , setValue :: T.Text
                     }
  deriving (Show, Eq)

-- | Options for the "config add-default" command to be parsed from the command-line.
data ConfigAddDefaultOptions
  = ConfigAddDefaultOptions { addDefaultDir  :: FilePath
                            , addDefaultName :: Maybe T.Text
                            }
  deriving (Show, Eq)

-- | Options for the "config" command to be parsed from the command-line.
data ConfigCommand
  = Print ConfigPrintOptions
  | Reset ConfigResetOptions
  | Set ConfigSetOptions
  | AddDefaultBookmark ConfigAddDefaultOptions
  deriving (Show, Eq)

-- | Options for the "check" command to be parsed from the command-line.
data CheckOptions
  = CheckOptions { checkConfig    :: Bool
                 , checkBookmarks :: Bool
                 }
  deriving (Show, Eq)

-- | Options for the "help" command to be parsed from the command-line.
newtype HelpOptions
  = HelpOptions { helpSubcommand :: Maybe T.Text }
  deriving (Show, Eq)

-- | The core data-type for the hoyo CLI. The 'Command' is parsed from the command-line,
-- then 'Hoyo.Command.runCommand' dispatches on the type.
data Command
  = Add AddOptions
  | Move MoveOptions
  | List ListOptions
  | Clear ClearOptions
  | Delete DeleteOptions
  | Refresh RefreshOptions
  | ConfigCmd ConfigCommand
  | Check CheckOptions
  | DefaultCommand
  | Help HelpOptions
  deriving (Show, Eq)

-- | Datatype for representing a command-line settings override.
data MaybeOverride = OverrideFalse | OverrideTrue | NoOverride | Conflict deriving
  ( Show
  , Eq
  )

-- | Config settings that can be overriden using command-line flags.
data OverrideOptions
  = OverrideOptions { overrideFailOnError         :: MaybeOverride
                    , overrideDisplayCreationTime :: MaybeOverride
                    , overrideEnableClearing      :: MaybeOverride
                    , overrideEnableReset         :: MaybeOverride
                    , overrideBackupBeforeClear   :: MaybeOverride
                    }
  deriving (Show, Eq)

-- | CLI options that can be set regardless of which command is run.
data GlobalOptions
  = GlobalOptions { globalConfigPath :: Maybe FilePath
                  , dataPath         :: Maybe FilePath
                  , overrides        :: OverrideOptions
                  }
  deriving (Show, Eq)

-- | The final result of parsing the CLI arguments. Contains a command and all
-- information for that command, and any global options that have been set.
data Options
  = Options { optCommand :: Command
            , optGlobals :: GlobalOptions
            }
  deriving (Show, Eq)

makeLenses ''Bookmark
makeLenses ''DefaultBookmark
makeLenses ''Env
flip makeLensesWith ''Config $
  lensRules
    & lensField .~ \_ _ n -> [TopName $ mkName $ '_' : nameBase n]
