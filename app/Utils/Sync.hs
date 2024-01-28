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
syncOpts = fmap Sync $
      flag' SyncPush (long "push" <> help "pushes the config to your git repo")
  <|> flag' SyncPull (long "pull" <> help "pulls the config from your git repo")
  <|> flag' SyncInit (long "init" <> help "pulls the config from the central git repo")

-- Algebras

runSync :: SyncOpts -> UtActionF ()
runSync = \case
  SyncPush -> undefined
  SyncPull -> undefined
  SyncInit -> syncInit

syncInit :: UtActionF ()
syncInit = runSysCmd $(embedStringFile "scripts/sync--init")
