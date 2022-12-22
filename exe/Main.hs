module Main where

import HoYo
import HoYo.Command
import HoYo.Config

import System.IO
import System.Exit

import Options.Applicative

globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
                  <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "config file"
                                <> value defaultConfigPath
                                <> help "Override the default config file"
                                <> showDefault)

addCommand :: Parser Options
addCommand = Options
              <$> (Add . AddOptions
                    <$> argument str (metavar "dir" <> help "Directory to bookmark"))
              <*> globalOptions

moveCommand :: Parser Options
moveCommand = Options
              <$> (Move . MoveOptions
                    <$> argument auto (metavar "index" <> help "Index of the bookmark to move to"))
              <*> globalOptions

listCommand :: Parser Options
listCommand = Options
              <$> pure (List ListOptions)
              <*> globalOptions

parseCommand :: Parser Options
parseCommand = hsubparser (
  command "add" (info addCommand (progDesc "Add a bookmark"))
  <> command "move" (info moveCommand (progDesc "Change directory using a bookmark"))
  <> command "list" (info listCommand (progDesc "List existing bookmarks"))
  ) <|> moveCommand

opts :: ParserInfo Options
opts = info (parseCommand <**> helper) (
          fullDesc
          <> progDesc "Set directory bookmarks for quick \"cd\"-like behaviour"
          )

main :: IO ()
main = do
  Options os globals <- execParser opts
  getConfig (configPath globals) >>= \case
    Left err    -> hPutStrLn stderr err >> exitFailure
    Right cfg   -> runHoYo (runCommand os) cfg >>= \case
      Left err  -> do hPutStrLn stderr err
                      exitFailure
      Right _   -> return ()
