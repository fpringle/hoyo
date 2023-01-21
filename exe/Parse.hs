module Parse where

import Complete
import HoYo

import qualified Data.Text as T
import Options.Applicative
import Text.Read

globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
                  <$> optional (strOption (long "config"
                                <> short 'c'
                                <> metavar "<file>"
                                <> help "Override the default config file"
                                <> showDefault
                                <> action "file"))
                  <*> optional (strOption (long "bookmarks"
                                <> short 'b'
                                <> metavar "<file>"
                                <> help "Override the default bookmarks file"
                                <> showDefault
                                <> action "file"))
                  <*> overrideOptions

overrideOptions :: Parser OverrideOptions
overrideOptions = OverrideOptions
                    <$> parseOverrideFailOnError
                    <*> parseOverrideDisplayCreationTime
                    <*> parseOverrideEnableClearing
                    <*> parseOverrideEnableReset

parseOverride :: Mod FlagFields Bool -> Mod FlagFields Bool -> Parser MaybeOverride
parseOverride posMod negMod = combOverride
                                <$> switch posMod
                                <*> switch negMod

parseOverrideFailOnError :: Parser MaybeOverride
parseOverrideFailOnError = parseOverride
                              (long "fail" <> help "Fail on error")
                              (long "nofail" <> help "Disable fail on error")

parseOverrideDisplayCreationTime :: Parser MaybeOverride
parseOverrideDisplayCreationTime = parseOverride
                                    (long "time" <> help "Display bookmark creation times")
                                    (long "notime" <> help "Hide bookmark creation times")

parseOverrideEnableClearing :: Parser MaybeOverride
parseOverrideEnableClearing = parseOverride
                                (long "enable-clear" <> help "Enable the 'clear' command")
                                (long "disable-clear" <> help "Disable the 'clear' command")

parseOverrideEnableReset :: Parser MaybeOverride
parseOverrideEnableReset = parseOverride
                              (long "enable-reset" <> help "Enable the 'config reset' command")
                              (long "disable-reset" <> help "Disable the 'config reset' command")

addCommand :: Parser Command
addCommand = Add <$> (
              AddOptions
                <$> strArgument (metavar "<dir>" <> help "Directory to bookmark" <> action "directory")
                <*> optional (strArgument (metavar "<name>" <> help "Optionally give a name to your bookmark"))
              )

bookmarkSearchTerm :: ReadM BookmarkSearchTerm
bookmarkSearchTerm = eitherReader $ \s ->
  SearchIndex <$> readEither s
  <|> Right (SearchName (T.pack s))

moveCommandMods :: Mod ArgumentFields BookmarkSearchTerm
moveCommandMods = metavar "<bookmark>"
                    <> help "Index or name of the bookmark to move to"
                    <> completer bookmarkCompleter

moveCommand :: Parser Command
moveCommand = Move . MoveOptions
                <$> argument bookmarkSearchTerm moveCommandMods

moveCommandHidden :: Parser Command
moveCommandHidden = Move . MoveOptions
                      <$> argument bookmarkSearchTerm (
                            moveCommandMods
                            <> internal
                          )

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

clearCommand :: Parser Command
clearCommand = pure (Clear ClearOptions)

deleteCommand :: Parser Command
deleteCommand =  Delete . DeleteOptions
                    <$> argument bookmarkSearchTerm (
                          metavar "<bookmark>"
                          <> help "Index or name of the bookmark to delete"
                          <> completer bookmarkCompleter
                        )

refreshCommand :: Parser Command
refreshCommand = pure (Refresh RefreshOptions)

configCommand :: Parser Command
configCommand = ConfigCmd <$> hsubparser (
  command "print" (info configPrintCommand (progDesc "Print hoyo config"))
  <> command "reset" (info configResetCommand (progDesc "Reset hoyo config"))
  <> command "set" (info configSetCommand (progDesc "Modify hoyo config"))
  <> command "add-default" (info configAddDefaultCommand (progDesc "Add a default bookmark"))
  )

configPrintCommand :: Parser ConfigCommand
configPrintCommand = pure (Print ConfigPrintOptions)

configResetCommand :: Parser ConfigCommand
configResetCommand = pure (Reset ConfigResetOptions)

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

noArgs :: CheckOptions -> CheckOptions
noArgs (CheckOptions False False) = CheckOptions True True
noArgs opts = opts

checkCommand :: Parser Command
checkCommand = Check . noArgs <$> (
                CheckOptions
                  <$> switch (long "config" <> short 'c' <> help "Check the config file")
                  <*> switch (long "bookmarks" <> short 'b' <> help "Check the bookmarks file")
                )

defaultCommand :: Parser Command
defaultCommand = pure DefaultCommand

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

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand <*> globalOptions

versionInfo :: String
versionInfo =
  "hoyo "
  <> versionString
  <> "\n\nCopyright (c) 2023, Frederick Pringle\n\nAll rights reserved."

versionOption :: Parser (a -> a)
versionOption = infoOption versionInfo (
                  long "version"
                  <> hidden
                  <> help "Display version information and exit"
                )

options :: ParserInfo Options
options = info (parseOptions <**> helper) (
          fullDesc
          <> header "Set directory bookmarks for quick \"cd\"-like behaviour"
          <> progDesc "For more help on a particular sub-command, run `hoyo <cmd> --help`."
          )

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
