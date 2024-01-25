module Utils.Obsidian (obsidCmd) where

import Options.Applicative
import Opts ( Command(Obsidian), ObsidianOpts(..) )
import Utils

obsidCmd :: Mod CommandFields Command
obsidCmd = mkCommand "ob" "Obsidian utils" $ Obsidian <$> subparser obsidOpts

obsidOpts :: Mod CommandFields ObsidianOpts
obsidOpts = mkCommand "with" "Runs cmd in vault dir" $ ObsidianWithVault <$> posArg "NAME" <*> posArg "CMD"
