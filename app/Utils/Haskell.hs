{-# LANGUAGE QuasiQuotes #-}

module Utils.Haskell (hsCmd, runHs) where

import Options.Applicative
import Opts
import Utils
import UtAction
import Text.RawString.QQ

-- Parsers

hsCmd :: Mod CommandFields Command
hsCmd = mkCommand "hs" "Haskell utils" $ Haskell <$> subparser hsOpts

hsOpts :: Mod CommandFields HaskellOpts
hsOpts =
     mkCommand "hp"   "Hpack utils"         (HsHpack <$> subparser hpackOpts)
  <> mkCommand "init" "Inits a new project" initProj

hpackOpts :: Mod CommandFields HPackOpts
hpackOpts =
     mkCommand "add" "Adds a dependency"    addDep
  <> mkCommand "rm"  "Removes a dependency" delDep

addDep :: Parser HPackOpts
addDep = HpackAdd <$> posArg "DEP"

delDep :: Parser HPackOpts
delDep = HpackDel <$> posArg "DEP"

initProj :: Parser HaskellOpts
initProj = HsInit 
  <$> posArg "NAME"
  <*> flag' Cabal (long "cabal")

-- Algebras

runHs :: HaskellOpts -> UtActionF ()
runHs = \case
  HsHpack (HpackAdd pkg) -> hpackAdd pkg
  HsHpack (HpackDel pkg) -> hpackDel pkg
  HsInit name Hpack -> hpackNew name
  HsInit name Cabal -> undefined

hpackAdd :: Text -> UtActionF ()
hpackAdd name = runSysCmd cmd >> hpackRegen where 
  cmd = var "name" name [r|awk -i inplace '
    /^dependencies:/ {
      in_list = 1 
    }
    in_list && match($0, /^(\s+)\S/, a) { 
      last_item = $0 
      indent = a[1]
    }
    in_list && !/^\s+\S/ && last_item { 
      print indent "- ${{name}}"
      in_list = 0
    }
    { print }
  ' package.yaml|]

hpackDel :: Text -> UtActionF ()
hpackDel name = runSysCmd cmd >> hpackRegen where
  cmd = var "name" name [r|awk -i inplace '
    !/^\s+-\s*${{name}}/ { 
      print 
    }
  ' package.yaml|]

hpackRegen :: UtActionF ()
hpackRegen = runSysCmd "hpack"

hpackNew :: Text -> UtActionF ()
hpackNew name = do
  template <- withCfgPath "template/haskell/package.yaml"
  printText $ var "name" name template
