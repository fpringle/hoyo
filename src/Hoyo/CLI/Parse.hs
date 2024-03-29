{-|
Module      : Hoyo.CLI.Parse
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Parse CLI arguments.
-}

module Hoyo.CLI.Parse (
  -- * Parsing CLI arguments and options
    options
  , parseOptions
  , parseCommand
  , globalOptions
  , overrideOptions

  -- * Parsing specific CLI commands
  , addCommand
  , moveCommand
  , listCommand
  , clearCommand
  , deleteCommand
  , checkCommand
  , defaultCommand

  -- ** Parsing sub-commands for hoyo config
  , configCommand
  , configPrintCommand
  , configResetCommand
  , configSetCommand
  , configAddDefaultCommand

  -- * Misc/Utility
  , splitArgs
  , showHelp
  ) where

import           Data.Char                       (isSpace)
import           Data.List
import qualified Data.Text                       as T

import           Hoyo.CLI.Complete
import           Hoyo.Command
import           Hoyo.Internal.Types
import           Hoyo.Internal.Version

import           Options.Applicative
import           Options.Applicative.Help.Chunk
import           Options.Applicative.Help.Pretty

import           System.Environment
import           System.Exit
import           System.Pager

import           Text.Read

-- | Parse global options: options that can be set on the command line
-- no matter what sub-command is being run.
--
-- For a complete list of the global options, see 'GlobalOptions' or
-- run @hoyo --help@.
globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
                  <$> optional (strOption (long "config-file"
                                <> short 'C'
                                <> metavar "<file>"
                                <> help "Override the default config file"
                                <> showDefault
                                <> action "file"))
                  <*> optional (strOption (long "bookmarks-file"
                                <> short 'B'
                                <> metavar "<file>"
                                <> help "Override the default bookmarks file"
                                <> showDefault
                                <> action "file"))
                  <*> overrideOptions

-- | Parse override options: options that override config settings.
-- This can be useful when you want to temporarily enable functionality
-- for one CLI run, but don't want to change it in the config file.
overrideOptions :: Parser OverrideOptions
overrideOptions = OverrideOptions
                    <$> parseOverrideFailOnError
                    <*> parseOverrideDisplayCreationTime
                    <*> parseOverrideEnableClearing
                    <*> parseOverrideEnableReset
                    <*> parseOverrideBackupBeforeClear

-- | Parse a single override option as a pair of switches.
-- For example 'parseOverrideFailOnError' is defined as
--
-- @
-- parseOverrideFailOnError :: 'Parser' 'MaybeOverride'
-- parseOverrideFailOnError = parseOverride
--                               (long "fail" <> help "Fail on error")
--                               (long "nofail" <> help "Disable fail on error")
-- @
--
-- and results in the pair of switches:
--
-- @
--  --fail                   Fail on error
--  --nofail                 Disable fail on error
-- @
parseOverride :: Mod FlagFields Bool -> Mod FlagFields Bool -> Parser MaybeOverride
parseOverride posMod negMod = combOverride
                                <$> switch posMod
                                <*> switch negMod

-- | Parse a 'MaybeOverride' corresponding to the "fail_on_error" config option.
parseOverrideFailOnError :: Parser MaybeOverride
parseOverrideFailOnError = parseOverride
                              (long "fail" <> help "Fail on error")
                              (long "nofail" <> help "Disable fail on error")

-- | Parse a 'MaybeOverride' corresponding to the "display_creation_time" config option.
parseOverrideDisplayCreationTime :: Parser MaybeOverride
parseOverrideDisplayCreationTime = parseOverride
                                    (long "time" <> help "Display bookmark creation times")
                                    (long "notime" <> help "Hide bookmark creation times")

-- | Parse a 'MaybeOverride' corresponding to the "enable_clear" config option.
parseOverrideEnableClearing :: Parser MaybeOverride
parseOverrideEnableClearing = parseOverride
                                (long "enable-clear" <> help "Enable the 'clear' command")
                                (long "disable-clear" <> help "Disable the 'clear' command")

-- | Parse a 'MaybeOverride' corresponding to the "enable_reset" config option.
parseOverrideEnableReset :: Parser MaybeOverride
parseOverrideEnableReset = parseOverride
                              (long "enable-reset" <> help "Enable the 'config reset' command")
                              (long "disable-reset" <> help "Disable the 'config reset' command")

-- | Parse a 'MaybeOverride' corresponding to the "backup_before_clear" config option.
parseOverrideBackupBeforeClear :: Parser MaybeOverride
parseOverrideBackupBeforeClear = parseOverride
                              (long "backup-before-clear" <> help "Backup the bookmarks file before running `hoyo clear`")
                              (long "no-backup-before-clear" <> help "Don't backup the bookmarks file before running `hoyo clear`")

-- | Parse options for the @hoyo add@ command.
addCommand :: Parser Command
addCommand = Add <$> (
              AddOptions
                <$> strArgument (metavar "<dir>" <> help "Directory to bookmark" <> action "directory")
                <*> optional (strArgument (metavar "<name>" <> help "Optionally give a name to your bookmark"))
              )

-- | Parse a 'BookmarkSearchTerm'. First tries to interpret the search term as a number
-- corresponding to a bookmark index; if that fails, treat the term as a name.
bookmarkSearchTerm :: ReadM BookmarkSearchTerm
bookmarkSearchTerm = eitherReader $ \s ->
  SearchIndex <$> readEither s
  <|> Right (SearchName (T.pack s))

-- | Parse options for the @hoyo move@ command.
moveCommand :: Parser Command
moveCommand = Move . MoveOptions
                <$> argument bookmarkSearchTerm (
                      metavar "<bookmark>"
                      <> help "Index or name of the bookmark to move to"
                      <> completer bookmarkCompleter
                      )

-- | Parse options for the @hoyo list@ command.
listCommand :: Parser Command
listCommand = List <$> (
                  ListOptions
                    <$> optional (strOption (
                          long "name"
                          <> short 'n'
                          <> metavar "<name>"
                          <> help "Search bookmarks by name"
                        ))
                    <*> optional (strOption (
                          long "dir"
                          <> short 'd'
                          <> metavar "<directory>"
                          <> help "Search bookmarks by directory"
                        ))
                    <*> switch (
                          long "json"
                          <> short 'j'
                          <> help "List bookmarks in JSON format"
                        )
                  )

-- | Parse options for the @hoyo clear@ command.
clearCommand :: Parser Command
clearCommand = pure (Clear ClearOptions)

-- | Parse options for the @hoyo delete@ command.
deleteCommand :: Parser Command
deleteCommand =  Delete . DeleteOptions
                    <$> argument bookmarkSearchTerm (
                          metavar "<bookmark>"
                          <> help "Index or name of the bookmark to delete"
                          <> completer bookmarkCompleter
                        )

-- | Parse options for the @hoyo refresh@ command.
refreshCommand :: Parser Command
refreshCommand = pure (Refresh RefreshOptions)

-- | Parse options for the @hoyo config@ command.
configCommand :: Parser Command
configCommand = ConfigCmd <$> hsubparser (
  command "print" (info configPrintCommand (progDesc "Print hoyo config"))
  <> command "reset" (info configResetCommand (progDesc "Reset hoyo config"))
  <> command "set" (info configSetCommand (progDesc "Modify hoyo config"))
  <> command "add-default" (info configAddDefaultCommand (progDesc "Add a default bookmark"))
  )

-- | Parse options for the @hoyo config print@ sub-command.
configPrintCommand :: Parser ConfigCommand
configPrintCommand = Print . ConfigPrintOptions
                        <$> switch (
                              long "json"
                              <> short 'j'
                              <> help "Print config in JSON format"
                            )

-- | Parse options for the @hoyo config reset@ sub-command.
configResetCommand :: Parser ConfigCommand
configResetCommand = pure (Reset ConfigResetOptions)

-- | Parse options for the @hoyo config set@ sub-command.
configSetCommand :: Parser ConfigCommand
configSetCommand = Set <$> (
                    ConfigSetOptions
                      <$> strArgument (
                            metavar "<key>"
                            <> help "Option to modify"
                            <> completer configKeyCompleter
                          )
                      <*> strArgument (
                            metavar "<value>"
                            <> help "Option value"
                            <> completer configValueCompleter
                          )
                    )

-- | Parse options for the @hoyo config add-default@ sub-command.
configAddDefaultCommand :: Parser ConfigCommand
configAddDefaultCommand = AddDefaultBookmark <$> (
                            ConfigAddDefaultOptions
                              <$> strArgument (
                                    metavar "<dir>"
                                    <> help "Directory to bookmark"
                                    <> action "directory"
                                  )
                              <*> optional (strArgument (
                                    metavar "<name>"
                                    <> help "Optionally give a name to your bookmark"
                                  ))
                          )

-- | `hoyo check` should be considered equivalent to `hoyo check --config --bookmarks`
noArgs :: CheckOptions -> CheckOptions
noArgs (CheckOptions False False) = CheckOptions True True
noArgs opts                       = opts

-- | Parse options for the @hoyo check@ command.
checkCommand :: Parser Command
checkCommand = Check . noArgs <$> (
                CheckOptions
                  <$> switch (long "config" <> short 'c' <> help "Check the config file")
                  <*> switch (long "bookmarks" <> short 'b' <> help "Check the bookmarks file")
                )

-- | Parse options for the @hoyo help@ command.
helpCommand :: Parser Command
helpCommand = Help . HelpOptions
                <$> optional (strArgument (
                                metavar "<command>"
                                <> help "Command to get help with"
                              )
                            )

-- | If hoyo is run with no arguments, we run the "default command" if it's set.
defaultCommand :: Parser Command
defaultCommand = pure DefaultCommand

-- | Parse a command and the arguments/options for that
-- command from the command-line arguments.
parseCommand :: Parser Command
parseCommand = (versionOption <*> hsubparser (
  command "add" (info addCommand (progDesc "Add a bookmark"))
  <> command "move" (info moveCommand (progDesc "Change directory using a bookmark"))
  <> command "list" (info listCommand (progDesc "List existing bookmarks"))
  <> command "clear" (info clearCommand (progDesc "Clear all bookmarks"))
  <> command "delete" (info deleteCommand (progDesc "Delete a bookmark"))
  <> command "refresh" (info refreshCommand (progDesc "Re-calculate bookmark indices"))
  <> command "config" (info configCommand (progDesc "View/manage hoyo config"))
  <> command "check" (info checkCommand (progDesc "Verify validity of config and bookmarks"))
  <> command "help" (info helpCommand (progDesc "Print a help message for the entire program or a specific command"))
  )) <|> defaultCommand

-- | Parse an 'Options' argument, which includes the command
-- to run and any global options.
parseOptions :: Parser Options
parseOptions = Options <$> parseCommand <*> globalOptions

-- | Version information printed by `hoyo --version`.
versionInfo :: String
versionInfo =
  "hoyo "
  <> versionString
  <> "\n\nCopyright (c) 2023, Frederick Pringle\n\nAll rights reserved."

-- | Add a hidden --version flag to the end of a parser.
versionOption :: Parser (a -> a)
versionOption = infoOption versionInfo (
                  long "version"
                  <> hidden
                  <> help "Display version information and exit"
                )

-- | A footer to display at the end of the long help message.
hoyoFooter :: Chunk Doc
hoyoFooter =
  let
    withTitle title = fmap ((string title .$.) . indent 2)
    bugText = paragraph "If something went wrong unexpectedly or you think there's a problem with hoyo, let me know! To report an issue, create a bug report at https://github.com/fpringle/hoyo/issues"
    docText = paragraph "To read the web documentation, visit https://github.com/fpringle/hoyo#readme"
  in vsepChunks [
        withTitle "Bugs:" bugText
      , withTitle "Online documentation:" docText
      ]

-- | A 'ParserInfo' object containing the necessary information for parsing
-- CLI commands and arguments, and displaying useful help text.
options :: ParserInfo Options
options = info (parseOptions <**> helper) (
          fullDesc
          <> header "Set directory bookmarks for quick \"cd\" behaviour"
          <> progDesc "For more help on a particular sub-command, run `hoyo <cmd> --help`."
          <> footer "for full documentation go to github"
          <> footerDoc (unChunk hoyoFooter)
          )

-- | Show the help message. Pass a non-'Nothing' argument to specify a command.
-- If the help message is longer than a page, pass it through the system's pager.
showHelp :: Maybe String -> IO ()
showHelp cmd = do
  let failure = parserFailure defaultPrefs options (ShowHelpText cmd) []
  progn <- getProgName
  let (msg, exit) = renderFailure failure progn
  let msg' = dropWhileEnd isSpace msg <> "\n"
  printOrPage $ T.pack msg'
  exitWith exit

-- | Split a string into arguments as they would be interpreted on the command line.
--
-- Adapted from [this StackOverflow comment](https://stackoverflow.com/a/64236441).
splitArgs :: T.Text -> [String]
splitArgs = splitArgs' False False False "" . T.unpack
  where
    splitArgs' :: Bool -> Bool -> Bool -> String -> String -> [String]
    splitArgs' _ _ True res [] = [res]
    splitArgs' _ _ False _ [] = []

    splitArgs' False _ _ res ('^':rest) = splitArgs' False False True (res <> "^") rest

    splitArgs' quoted True _ res (chr:rest) = splitArgs' quoted False True (res <> [chr]) rest

    splitArgs' quoted escaped _ res ('"':rest) = splitArgs' (not quoted) escaped True res rest

    splitArgs' quoted False started res ('\\':'"':rest) = splitArgs' quoted True started res ('"':rest)

    splitArgs' False escaped started res (' ':rest) = [res | started] <> splitArgs' False escaped False "" rest

    splitArgs' quoted escaped _ res (chr:rest) = splitArgs' quoted escaped True (res <> [chr]) rest
