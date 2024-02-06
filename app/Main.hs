module Main where

import Options.Applicative
import UtOpts
import Utils.Sync
import Utils.Config
import Utils.Haskell
import Utils.EchoCP ( ecpCmd, runEcp )
import Utils.JetBrains
import Utils.With
import Utils.License
import UtAction

main :: IO ()
main = customExecParser (prefs showHelpOnEmpty) optsParser <&> coerce >>= runActions . \case
  Sync opts -> runSync opts
  Config opts -> runConfig opts
  Haskell opts -> runHs opts
  EchoCP key -> runEcp key
  JetBrains opts -> runJB opts
  With opts -> runWith opts
  License opts -> runLicense opts

optsParser :: ParserInfo UtOpts
optsParser = info
  (helper
    <*> programOptions)
  (fullDesc
    <> progDesc "ut."
    <> header   "ut — some random utils for myself")

programOptions :: Parser UtOpts
programOptions = UtOpts <$> subparser
  ( syncCmd <> configCmd <> hsCmd <> ecpCmd <> jbCmd <> withCmd <> licenseCmd
  )
