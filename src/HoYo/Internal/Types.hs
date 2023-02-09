{-|
Module      : HoYo.Internal.Types
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Types used by all the main HoYo.* modules.
-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wno-missing-signatures #-}
{-# OPTIONS_HADDOCK prune #-}

module HoYo.Internal.Types where

import Control.Monad.Except (ExceptT, MonadError(..))
import Control.Monad.IO.Class
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT)

import Lens.Micro.TH

import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time

import System.IO.Error

-- | A 'T.Text' version of 'FilePath'.
type TFilePath = T.Text

-- | The main hoyo read-only environment. Contains the current saved bookmarks,
-- the current hoyo configuration, and the file locations for each.
data Env = Env {
  _bookmarks        :: !Bookmarks
  , _bookmarksPath  :: !TFilePath
  , _config         :: !Config
  , _configPath     :: !TFilePath
  } deriving Show

-- | Bookmark a directory for easy @cd@. A bookmark remembers the directory,
-- the index, the creation time, and optionally a user-specified nickname
-- for the bookmark.
data Bookmark = Bookmark {
  _bookmarkDirectory        :: !TFilePath
  , _bookmarkIndex          :: !Int
  , _bookmarkCreationTime   :: !ZonedTime
  , _bookmarkName           :: !(Maybe T.Text)
  } deriving Show

-- | Default bookmarks to save at init. A default bookmark remembers the directory
-- and optionally a user-specified nickname for the bookmark.
data DefaultBookmark = DefaultBookmark {
  _defaultBookmarkDirectory        :: !TFilePath
  , _defaultBookmarkName           :: !(Maybe T.Text)
  } deriving (Show, Eq)

-- | Wrapper for @['Bookmark']@.
newtype Bookmarks = Bookmarks { unBookmarks :: [Bookmark] }
  deriving Show

-- | Data-type for represting a bookmark search. You can either search
-- by index or by name. Used by the @delete@ and @move@ commands.
data BookmarkSearchTerm =
  SearchIndex Int
  | SearchName T.Text
  deriving (Show, Eq)

-- | The types of config values allowed in the HoYo config.
data ConfigValueType =
    TBool
  | TDefaultBookmark
  | TCommand
  | TList ConfigValueType
  | TMaybe ConfigValueType

-- | Values in the HoYo config. Using a GADT parameterised by 'ConfigValueType'
-- gives us stricter type safety.
data ConfigValue (t :: ConfigValueType) where
  BoolV             :: Bool -> ConfigValue 'TBool
  DefaultBookmarkV  :: DefaultBookmark -> ConfigValue 'TDefaultBookmark
  CommandV          :: T.Text -> ConfigValue 'TCommand

  ListOfV           :: forall (a :: ConfigValueType) . [ConfigValue a] -> ConfigValue ('TList a)
  MaybeV            :: forall (a :: ConfigValueType) . Maybe (ConfigValue a) -> ConfigValue ('TMaybe a)

instance Show (ConfigValue (t :: ConfigValueType)) where
  show (BoolV bool) = show bool
  show (DefaultBookmarkV bm) = show bm
  show (CommandV t) = show t
  show (ListOfV xs) = "[" <> intercalate ", " (map show xs) <> "]"
  show (MaybeV xs) = show xs

instance Eq (ConfigValue (t :: ConfigValueType)) where
  BoolV b1            == BoolV b2             = b1 == b2
  DefaultBookmarkV b1 == DefaultBookmarkV b2  = b1 == b2
  CommandV b1         == CommandV b2          = b1 == b2
  ListOfV b1          == ListOfV b2           = b1 == b2
  MaybeV b1           == MaybeV b2            = b1 == b2

-- | Existential wrapper around 'ConfigValue'.
data AnyConfigValue = forall (t :: ConfigValueType) . AnyConfigValue (ConfigValue t)

-- | A representation of hoyo settings.
data Config = Config {
  _failOnError            :: !(ConfigValue 'TBool)
  , _displayCreationTime  :: !(ConfigValue 'TBool)
  , _enableClearing       :: !(ConfigValue 'TBool)
  , _enableReset          :: !(ConfigValue 'TBool)
  , _backupBeforeClear    :: !(ConfigValue 'TBool)
  , _defaultBookmarks     :: !(ConfigValue ('TList 'TDefaultBookmark))
  , _defaultCommand       :: !(ConfigValue ('TMaybe 'TCommand))
  } deriving (Show, Eq)

-- | 'HoYoMonad' is the main monad stack for the hoyo program. It's essentially a wrapper
-- around @ExceptT T.Text (ReaderT Env IO)@: in other words,
-- @HoYoMonad a@ is equivalent to @Env -> IO (Either T.Text a)@
newtype HoYoMonad a = HoYoMonad {
  unHoYo :: ExceptT T.Text (ReaderT Env IO) a
  } deriving (Functor, Applicative, Monad, MonadError T.Text, MonadReader Env)

instance MonadIO HoYoMonad where
  liftIO m = HoYoMonad (liftIO $ tryIOError m) >>= \case
    Left err      -> throwError ("IO error: " <> T.pack (show err))
    Right result  -> return result

-- | The result of executing a command. Currently only used meaningfully
-- by 'HoYo.Command.runDefaultCommand'.
data ExecResult =
  Done
  | ShowHelp
  | ReRun T.Text

-- | Options for the "add" command to be parsed from the command-line.
data AddOptions = AddOptions {
  addDirectory  :: TFilePath
  , addName     :: Maybe T.Text
  } deriving (Show, Eq)

-- | Options for the "move" command to be parsed from the command-line.
newtype MoveOptions = MoveOptions {
  moveSearch :: BookmarkSearchTerm
  } deriving (Show, Eq)

-- | Options for the "list" command to be parsed from the command-line.
data ListOptions = ListOptions {
  listFilterName                :: Maybe T.Text
  , listFilterDirectoryInfix    :: Maybe T.Text
  } deriving (Show, Eq)

-- | Options for the "clear" command to be parsed from the command-line.
data ClearOptions = ClearOptions {
  } deriving (Show, Eq)

-- | Options for the "delete" command to be parsed from the command-line.
newtype DeleteOptions = DeleteOptions {
  deleteSearch :: BookmarkSearchTerm
  } deriving (Show, Eq)

-- | Options for the "refresh" command to be parsed from the command-line.
data RefreshOptions = RefreshOptions {
  } deriving (Show, Eq)

-- | Options for the "config print" command to be parsed from the command-line.
data ConfigPrintOptions = ConfigPrintOptions {
  } deriving (Show, Eq)

-- | Options for the "config reset" command to be parsed from the command-line.
data ConfigResetOptions = ConfigResetOptions {
  } deriving (Show, Eq)

-- | Options for the "config set" command to be parsed from the command-line.
data ConfigSetOptions = ConfigSetOptions {
  setKey        :: T.Text
  , setValue    :: T.Text
  } deriving (Show, Eq)

-- | Options for the "config add-default" command to be parsed from the command-line.
data ConfigAddDefaultOptions = ConfigAddDefaultOptions {
  addDefaultDir       :: TFilePath
  , addDefaultName    :: Maybe T.Text
  } deriving (Show, Eq)

-- | Options for the "config" command to be parsed from the command-line.
data ConfigCommand =
  Print ConfigPrintOptions
  | Reset ConfigResetOptions
  | Set ConfigSetOptions
  | AddDefaultBookmark ConfigAddDefaultOptions
  deriving (Show, Eq)

-- | Options for the "check" command to be parsed from the command-line.
data CheckOptions = CheckOptions {
  checkConfig         :: Bool
  , checkBookmarks    :: Bool
  } deriving (Show, Eq)

-- | The core data-type for the hoyo CLI. The 'Command' is parsed from the command-line,
-- then 'HoYo.Command.runCommand' dispatches on the type.
data Command =
  Add AddOptions
  | Move MoveOptions
  | List ListOptions
  | Clear ClearOptions
  | Delete DeleteOptions
  | Refresh RefreshOptions
  | ConfigCmd ConfigCommand
  | Check CheckOptions
  | DefaultCommand
  deriving (Show, Eq)


makeLenses ''Bookmark
makeLenses ''DefaultBookmark
makeLenses ''Env
