module Utils.Sync (syncCmd, runSync) where

import Options.Applicative
import Opts
import Utils
import UtAction (UtActionF, runSysCmd, withAbsTPath)

-- Parsers

syncCmd :: Mod CommandFields Command
syncCmd = mkCommand "sync" "Syncs templates to your xdg-spec config dir" syncOpts

syncOpts :: Parser Command
syncOpts = pure Sync

-- Algebras

runSync :: UtActionF ()
runSync = withAbsTPath "scripts/sync" >>= runSysCmd
