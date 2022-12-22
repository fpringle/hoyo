{-# LANGUAGE LambdaCase #-}
module Main where

import HoYo
import HoYo.Command
import HoYo.Config

import System.IO
import System.Exit

import Options.Applicative

addCommand :: Parser Command
addCommand = Add . AddOptions <$> argument str (metavar "dir" <> help "Directory to bookmark")

moveCommand :: Parser Command
moveCommand = Move . MoveOptions <$> argument auto (metavar "index" <> help "Index of the bookmark to move to")

listCommand :: Parser Command
listCommand = pure (List ListOptions)

parseCommand :: Parser Command
parseCommand = hsubparser (
  command "add" (info addCommand (progDesc "Add a bookmark"))
  <> command "move" (info moveCommand (progDesc "Change directory using a bookmark"))
  <> command "list" (info listCommand (progDesc "List existing bookmarks"))
  ) <|> moveCommand

opts :: ParserInfo Command
opts = info (parseCommand <**> helper) (
          fullDesc
          <> progDesc "Set directory bookmarks for quick \"cd\"-like behaviour"
          )

main :: IO ()
main = do
  options <- execParser opts
  cfg <- getConfig
  runHoYo (runCommand options) cfg >>= \case
    Left err  -> do hPutStrLn stderr err
                    exitFailure
    Right _   -> return ()
