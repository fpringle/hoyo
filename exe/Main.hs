module Main where

import HoYo
import HoYo.Command
import HoYo.Env

import System.IO
import System.Exit
import System.Directory

import Options.Applicative

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

addCommand :: Parser Options
addCommand = Options
              <$> (Add . AddOptions
                    <$> argument str (metavar "DIR" <> help "Directory to bookmark"))
              <*> globalOptions

moveCommand :: Parser Options
moveCommand = Options
              <$> (Move . MoveOptions
                    <$> argument auto (metavar "INDEX" <> help "Index of the bookmark to move to"))
              <*> globalOptions

listCommand :: Parser Options
listCommand = Options
              <$> pure (List ListOptions)
              <*> globalOptions

clearCommand :: Parser Options
clearCommand = Options
                <$> pure (Clear ClearOptions)
                <*> globalOptions

deleteCommand :: Parser Options
deleteCommand =  Options
                  <$> (Delete . DeleteOptions
                        <$> argument auto (metavar "INDEX" <> help "Index of the bookmark to delete"))
                  <*> globalOptions

refreshCommand :: Parser Options
refreshCommand = Options
                <$> pure (Refresh RefreshOptions)
                <*> globalOptions

parseCommand :: Parser Options
parseCommand = hsubparser (
  command "add" (info addCommand (progDesc "Add a bookmark"))
  <> command "move" (info moveCommand (progDesc "Change directory using a bookmark"))
  <> command "list" (info listCommand (progDesc "List existing bookmarks"))
  <> command "clear" (info clearCommand (progDesc "Clear all bookmarks"))
  <> command "delete" (info deleteCommand (progDesc "Delete a bookmark"))
  <> command "refresh" (info refreshCommand (progDesc "Re-calculate bookmark indices"))
  ) <|> moveCommand

opts :: ParserInfo Options
opts = info (parseCommand <**> helper) (
          fullDesc
          <> progDesc "Set directory bookmarks for quick \"cd\"-like behaviour"
          )

main :: IO ()
main = do
  Options os globals <- execParser opts

  sFp <- case configPath globals of
    Nothing -> getXdgDirectory XdgConfig "hoyo/config.toml"
    Just d  -> return d
  bFp <- case dataPath globals of
    Nothing -> getXdgDirectory XdgData "hoyo/bookmarks.toml"
    Just d  -> return d

  getEnv bFp sFp >>= \case
    Left err    -> hPutStrLn stderr err >> exitFailure
    Right env   -> runHoYo (runCommand os) env >>= \case
      Left err  -> do hPutStrLn stderr err
                      exitFailure
      Right _   -> return ()
