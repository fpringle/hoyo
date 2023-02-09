{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore "Use list comprehension" -}

module HoYo.Test.CLI.Parse where

import HoYo
import HoYo.CLI.Parse
import HoYo.Test.Gen ()

import qualified Data.Text as T
import Options.Applicative as O
import Test.QuickCheck

formatSearchTerm :: BookmarkSearchTerm -> String
formatSearchTerm (SearchIndex idx) = show idx
formatSearchTerm (SearchName name) = T.unpack name

singletonUnpack :: T.Text -> [String]
singletonUnpack t = [T.unpack t]

maybeSingletonUnpack :: Maybe T.Text -> [String]
maybeSingletonUnpack = maybe [] singletonUnpack

maybeSingletonUnpackWithPrefix :: [String] -> Maybe T.Text -> [String]
maybeSingletonUnpackWithPrefix pref = maybe [] (\t -> pref <> [T.unpack t])

formatCommand :: Command -> [String]
formatCommand (Add (AddOptions d n)) = "add" : T.unpack d : maybeSingletonUnpack n
formatCommand (Move opts) = ["move", formatSearchTerm $ moveSearch opts]
formatCommand (List (ListOptions n d)) = ["list"]
                                      <> maybeSingletonUnpackWithPrefix ["--name"] n
                                      <> maybeSingletonUnpackWithPrefix ["--dir"] d
formatCommand (Clear ClearOptions) = ["clear"]
formatCommand (Delete opts) = ["delete", formatSearchTerm $ deleteSearch opts]
formatCommand (Refresh RefreshOptions) = ["refresh"]
formatCommand (ConfigCmd cmd) = "config" : formatConfigCommand cmd
formatCommand (Check (CheckOptions c b)) = ["check"]
                                        <> (if c then ["--config"] else [])
                                        <> (if b then ["--bookmarks"] else [])
formatCommand DefaultCommand = []

formatConfigCommand :: ConfigCommand -> [String]
formatConfigCommand (Print ConfigPrintOptions) = ["print"]
formatConfigCommand (Reset ConfigResetOptions) = ["reset"]
formatConfigCommand (Set opts) = ["set"
                                , T.unpack (setKey opts)
                                , T.unpack (setValue opts)
                                ]
formatConfigCommand (AddDefaultBookmark (ConfigAddDefaultOptions d n))
  = "add-default" : T.unpack d : maybeSingletonUnpack n

formatGlobals :: GlobalOptions -> [String]
formatGlobals (GlobalOptions c d o) = maybeSingletonUnpackWithPrefix ["--config-file"] c
                                   <> maybeSingletonUnpackWithPrefix ["--bookmarks-file"] d
                                   <> formatOverrides o

formatOverrides :: OverrideOptions -> [String]
formatOverrides (OverrideOptions f t c r) = formatOverride "fail" "nofail" f
                                         <> formatOverride "time" "notime" t
                                         <> formatOverride "enable-clear" "disable-clear" c
                                         <> formatOverride "enable-reset" "disable-reset" r

formatOverride :: String -> String -> MaybeOverride -> [String]
formatOverride _ no OverrideFalse = ["--" <> no]
formatOverride yes _ OverrideTrue = ["--" <> yes]
formatOverride _ _ NoOverride     = []
formatOverride yes no Conflict    = ["--" <> no, "--" <> yes]

formatOptions :: Options -> [String]
formatOptions (Options c g) = formatCommand c <> formatGlobals g

formatArgs :: [String] -> String
formatArgs = unwords . map quoteStrings
  where quoteStrings s | ' ' `elem` s = "\"" <> s <> "\""
                       | otherwise  = s

testParserResult :: Options -> ParserResult Options -> Property
testParserResult a (O.Success b) = counterexample (formatArgs $ formatOptions b) $ a === b
testParserResult _ _ = property False

prop_OptionsRoundTrip :: Options -> Property
prop_OptionsRoundTrip opts =
  let args = formatOptions opts
      result = execParserPure defaultPrefs options args
  in counterexample (formatArgs args) $ testParserResult opts result

return []
parseTests :: IO Bool
parseTests = $quickCheckAll
