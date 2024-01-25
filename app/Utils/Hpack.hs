module Utils.Hpack (hpackCmd) where

import Options.Applicative
import Opts
import Utils

hpackCmd :: Mod CommandFields Command
hpackCmd = mkCommand "hp" "Hpack utils" $ Hpack <$> subparser hpackOpts

hpackOpts :: Mod CommandFields HPackOpts
hpackOpts =
     mkCommand "add" "Adds a dependency"    addDep
  <> mkCommand "del" "Removes a dependency" delDep
  <> mkCommand "new" "Inits a new project"  initProj

addDep :: Parser HPackOpts
addDep = HpackAdd <$> posArg "DEP"

delDep :: Parser HPackOpts
delDep = HpackDel <$> posArg "DEP"

initProj :: Parser HPackOpts
initProj = HpackNew <$> posArg "NAME"
