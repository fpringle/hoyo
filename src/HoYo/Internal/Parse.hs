{-|
Module      : HoYo.Internal.Parse
Copyright   : (c) Frederick Pringle, 2023
License     : BSD-3-Clause
Maintainer  : freddyjepringle@gmail.com

Parse CLI arguments.
-}

module HoYo.Internal.Parse where

import HoYo.CLI.Complete
import HoYo.Internal.Command
import HoYo.Internal.Types
import HoYo.Internal.Version

import qualified Data.Text as T
import Options.Applicative
import Text.Read

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

-- | Parse mods + bookmark completer used in 'moveCommand' and 'moveCommandHidden'.
moveCommandMods :: Mod ArgumentFields BookmarkSearchTerm
moveCommandMods = metavar "<bookmark>"
                    <> help "Index or name of the bookmark to move to"
                    <> completer bookmarkCompleter

-- | Parse options for the @hoyo move@ command.
moveCommand :: Parser Command
moveCommand = Move . MoveOptions
                <$> argument bookmarkSearchTerm moveCommandMods

-- | When hoyo is run without a command but with a search term (e.g. `hoyo docs`),
-- it is interpreted as a move command - so the example above would be interpreted
-- as `hoyo move docs`. We hide this from the CLI help message because it's quite
-- ugly.
moveCommandHidden :: Parser Command
moveCommandHidden = Move . MoveOptions
                      <$> argument bookmarkSearchTerm (
                            moveCommandMods
                            <> internal
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
configPrintCommand = pure (Print ConfigPrintOptions)

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
noArgs opts = opts

-- | Parse options for the @hoyo check@ command.
checkCommand :: Parser Command
checkCommand = Check . noArgs <$> (
                CheckOptions
                  <$> switch (long "config" <> short 'c' <> help "Check the config file")
                  <*> switch (long "bookmarks" <> short 'b' <> help "Check the bookmarks file")
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
  )) <|> moveCommandHidden
     <|> defaultCommand

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

-- | A 'ParserInfo' object containing the necessary information for parsing
-- CLI commands and arguments, and displaying useful help text.
options :: ParserInfo Options
options = info (parseOptions <**> helper) (
          fullDesc
          <> header "Set directory bookmarks for quick \"cd\"-like behaviour"
          <> progDesc "For more help on a particular sub-command, run `hoyo <cmd> --help`."
          )

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
