module Main where

import HoYo
import HoYo.CLI.Parse

import Control.Monad (forM_)
import qualified Data.Text as T
import Options.Applicative
import System.Environment (withProgName)
import System.Exit


failure :: T.Text -> IO ()
failure err = do
  printStderr ("Error: " <> err)
  exitWith (ExitFailure 1)

main :: IO ()
main = withProgName "hoyo" $ do
  opts@(Options _ globals) <- execParser options
  forM_ (verifyOverrides $ overrides globals) failure

  sFp <- maybe defaultConfigPath return $ globalConfigPath globals
  bFp <- maybe defaultBookmarksPath return $ dataPath globals

  getEnvAndRunCommand opts bFp sFp -- >>= \case
    -- Done      -> return ()
    -- ShowHelp  -> handleParseResult (Failure $ parserFailure defaultPrefs options (ShowHelpText Nothing) [])
    -- ReRun cmd -> withArgs (splitArgs cmd) main
