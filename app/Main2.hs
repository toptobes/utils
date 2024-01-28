module Main2 where

import Options.Applicative
import UtOpts
import Utils.Sync
import Utils.Config
import Utils.Haskell
import Utils.EchoCP
import Utils.JetBrains
import Utils.Obsidian
import Utils.License
import UtAction

main :: IO ()
main = execParser optsParser <&> coerce >>= runActions . \case
  Sync opts -> runSync opts
  Config opts -> runConfig opts
  Haskell opts -> runHs opts
  EchoCP key -> runEcp key
  JetBrains opts -> runJB opts
  Obsidian opts -> runObsid opts
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
  ( syncCmd <> configCmd <> hsCmd <> ecpCmd <> jbCmd <> obsidCmd <> licenseCmd
  )
