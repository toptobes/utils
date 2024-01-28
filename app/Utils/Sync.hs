{-# LANGUAGE TemplateHaskell #-}

module Utils.Sync (syncCmd, runSync) where

import Options.Applicative
import Opts
import Utils
import UtAction
import UtConfig
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
  SyncPush -> syncPush
  SyncPull -> syncPull
  SyncInit -> syncInit

syncInit :: UtActionF ()
syncInit = runSysCmd $ "#!/bin/sh\nset -- toptobes/utils mistress\n" <> $(embedStringFile "scripts/sync--pull")

withRepo :: (Text -> Text -> UtActionF a) -> UtActionF a
withRepo fn = withCfg <&> repo >>= \case
  Repo (Just path) (Just branch) -> fn path branch
  _ -> panik "Both repo.path and repo.branch must be set..."

syncPush :: UtActionF ()
syncPush = withRepo $ \gpath gbranch -> do
  path <- withCfgPath "scripts/sync--push"
  runSysCmd $ unwords [path, gpath, gbranch]

syncPull :: UtActionF ()
syncPull = withRepo $ \gpath gbranch -> do
  path <- withCfgPath "scripts/sync--pull"
  runSysCmd $ unwords [path, gpath, gbranch]
