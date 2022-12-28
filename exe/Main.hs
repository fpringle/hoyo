{-# LANGUAGE TemplateHaskell #-}
module Main where

import HoYo

import qualified Data.Text as T
import Text.Read

import Control.Monad

import System.Environment (withProgName)
-- import System.IO
import System.Directory
import System.Exit

import Options.Applicative

import Data.Version.Package

globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
                  <$> optional (strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "Override the default config file"
                                <> showDefault))
                  <*> optional (strOption (long "bookmarks"
                                <> short 'b'
                                <> metavar "FILE"
                                <> help "Override the default bookmarks file"
                                <> showDefault))
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
                <$> strArgument (metavar "DIR" <> help "Directory to bookmark")
                <*> optional (strArgument (metavar "NAME" <> help "Optionally give a name to your bookmark"))
              )

bookmarkSearchTerm :: ReadM BookmarkSearchTerm
bookmarkSearchTerm = eitherReader $ \s ->
  SearchIndex <$> readEither s
  <|> Right (SearchName (T.pack s))

moveCommand :: Parser Command
moveCommand = Move . MoveOptions
                <$> argument bookmarkSearchTerm (metavar "BOOKMARK" <> help "Index or name of the bookmark to move to")

listCommand :: Parser Command
listCommand = List <$> (
                  ListOptions
                    <$> optional (strOption (
                          long "name"
                          <> short 'n'
                          <> metavar "NAME"
                          <> help "Search bookmarks by name"
                        ))
                    <*> optional (strOption (
                          long "dir"
                          <> short 'd'
                          <> metavar "DIRECTORY"
                          <> help "Search bookmarks by directory"
                        ))
                  )

clearCommand :: Parser Command
clearCommand = pure (Clear ClearOptions)

deleteCommand :: Parser Command
deleteCommand =  Delete . DeleteOptions
                    <$> argument bookmarkSearchTerm (metavar "BOOKMARK" <> help "Index or name of the bookmark to delete")

refreshCommand :: Parser Command
refreshCommand = pure (Refresh RefreshOptions)

configCommand :: Parser Command
configCommand = ConfigCmd <$> hsubparser (
  command "print" (info configPrintCommand (progDesc "Print hoyo config"))
  <> command "reset" (info configResetCommand (progDesc "Reset hoyo config"))
  <> command "set" (info configSetCommand (progDesc "Modify hoyo config"))
  )

configPrintCommand :: Parser ConfigCommand
configPrintCommand = pure (Print ConfigPrintOptions)

configResetCommand :: Parser ConfigCommand
configResetCommand = pure (Reset ConfigResetOptions)

configSetCommand :: Parser ConfigCommand
configSetCommand = Set <$> (
                    ConfigSetOptions
                      <$> argument str (metavar "KEY" <> help "Option to modify")
                      <*> argument str (metavar "VALUE" <> help "Option value")
                    )

parseCommand :: Parser Command
parseCommand = versionOption <*> hsubparser (
  command "add" (info addCommand (progDesc "Add a bookmark"))
  <> command "move" (info moveCommand (progDesc "Change directory using a bookmark"))
  <> command "list" (info listCommand (progDesc "List existing bookmarks"))
  <> command "clear" (info clearCommand (progDesc "Clear all bookmarks"))
  <> command "delete" (info deleteCommand (progDesc "Delete a bookmark"))
  <> command "refresh" (info refreshCommand (progDesc "Re-calculate bookmark indices"))
  <> command "config" (info configCommand (progDesc "View/manage hoyo config"))
  )

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand <*> globalOptions

versionOption :: Parser (a -> a)
versionOption = infoOption versionInfo (long "version")

options :: ParserInfo Options
options = info (parseOptions <**> helper) (
          fullDesc
          <> progDesc "Set directory bookmarks for quick \"cd\"-like behaviour"
          )

failure :: T.Text -> IO ()
failure err = do
  printStderr ("Error: " <> err)
  exitWith (ExitFailure 1)

versionString :: String
versionString = $$(packageVersionStringTH "hoyo.cabal")

versionInfo :: String
versionInfo =
  "hoyo "
  <> versionString
  <> "\n\nCopyright (c) 2022, Frederick Pringle\n\nAll rights reserved."

main :: IO ()
main = withProgName "hoyo" $ do
  -- Options os globals <- getOptions
  Options os globals <- execParser options
  forM_ (verifyOverrides $ overrides globals) failure

  sFp <- case globalConfigPath globals of
    Nothing -> T.pack <$> getXdgDirectory XdgConfig "hoyo/config.toml"
    Just d  -> return d
  bFp <- case dataPath globals of
    Nothing -> T.pack <$> getXdgDirectory XdgData "hoyo/bookmarks.toml"
    Just d  -> return d

  getEnv bFp sFp >>= \case
    Left err    -> failure err
    Right env   -> do
      let overridenEnv = overrideEnv (overrides globals) env
      runHoYo (runCommand os) overridenEnv >>= \case
        Left err  -> failure err
        Right _   -> return ()
