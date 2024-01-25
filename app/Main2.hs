module Main2 where

import Options.Applicative
import Opts
import Utils.Sync
import Utils.Config
import Utils.Hpack
import Utils.EchoCP
import Utils.JetBrains
import Utils.Obsidian
import UtAction

main :: IO ()
main = execParser optsParser <&> coerce >>= runActions . \case
  Sync -> runSync
  Config opts -> runConfig opts
  _ -> error "todo lol"

optsParser :: ParserInfo Opts
optsParser = info
  (helper 
    <*> programOptions)
  (fullDesc
    <> progDesc "ut."
    <> header   "ut — some random utils for myself")

programOptions :: Parser Opts
programOptions = Opts <$> subparser
  ( syncCmd <> configCmd <> hpackCmd <> ecpCmd <> jbCmd <> obsidCmd
  )
