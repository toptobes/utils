module Utils.Config (configCmd, runConfig) where

import Options.Applicative
import Opts
import Utils
import UtAction
import UtConfig

-- Parsers

configCmd :: Mod CommandFields Command
configCmd =  mkCommand "conf" "Works with the config" $ Config <$> subparser configOpts

configOpts :: Mod CommandFields ConfigOpts
configOpts =
     mkCommand "path" "Prints config file path"  ppathConfigOpt
  <> mkCommand "list" "Lists the current config" listConfigOpt
  <> mkCommand "get"  "Gets the given value"     getConfigOpt
  <> mkCommand "set"  "Sets the given value"     setConfigOpt

ppathConfigOpt :: Parser ConfigOpts
ppathConfigOpt = pure CfgPath

listConfigOpt :: Parser ConfigOpts
listConfigOpt = pure ListCfg

getConfigOpt :: Parser ConfigOpts
getConfigOpt = GetCfgVal <$> posArg "CONFIG"

setConfigOpt :: Parser ConfigOpts
setConfigOpt = SetCfgVal <$> posArg "CONFIG" <*> posArg "NEW_VAL"

-- Algebras

runConfig :: ConfigOpts -> UtActionF ()
runConfig = \case 
  CfgPath -> printConfigPath
  GetCfgVal path -> lookupConfig path
  _ -> error "todo lol"

printConfigPath :: UtActionF ()
printConfigPath = withCfgPath >>= printText

lookupConfig :: Text -> UtActionF ()
lookupConfig path = (withCfg <&> lookupCfg path) >>= printText
