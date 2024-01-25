{-# LANGUAGE TemplateHaskell #-}

module Utils.Sync (syncCmd, runSync) where

import Options.Applicative
import Opts
import Utils
import UtAction
import Data.FileEmbed

-- Parsers

syncCmd :: Mod CommandFields Command
syncCmd = mkCommand "sync" "Syncs templates to your xdg-spec config dir" syncOpts

syncOpts :: Parser Command
syncOpts = pure Sync

-- Algebras

runSync :: UtActionF ()
runSync = runSysCmd $(embedStringFile "templates/scripts/sync")
