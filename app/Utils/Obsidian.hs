module Utils.Obsidian (obsidCmd, runObsid) where

import Options.Applicative
import UtOpts
import Utils
import UtAction 
import UtConfig
import Data.Map.Strict qualified as M

-- Parsers

obsidCmd :: Mod CommandFields Command
obsidCmd = mkCommand "ob" "Obsidian utils" $ Obsidian <$> obsidOpts

obsidOpts :: Parser ObsidianOpts
obsidOpts = ObsidianWithVault 
  <$> strOption (long "with" <> short 'w' <> metavar "VAULT" <> help "Name of vault to use")
  <*> posArg "CMD"

-- Algebras

runObsid :: ObsidianOpts -> UtActionF ()
runObsid (ObsidianWithVault name cmd) = withCfg <&> M.lookup name . vaults >>= \case
  Just path -> runSysCmd $ "cd '" <> path <> "' && " <> cmd
  Nothing -> panik $ "No vault with name '" <> name <> "' found..."
