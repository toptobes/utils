module Utils.Config (configCmd, runConfig) where

import Options.Applicative
import Opts
import Utils
import UtAction
import UtConfig
import Data.Text qualified as T
import Data.Map.Strict qualified as M

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
  ListCfg -> listConfig
  GetCfgVal path -> lookupConfig path
  SetCfgVal path new -> setConfig path new

printConfigPath :: UtActionF ()
printConfigPath = withCfgPath >>= printText

lookupConfig :: Text -> UtActionF ()
lookupConfig path = (withCfg <&> getPath path) >>= printText

setConfig :: Text -> Text -> UtActionF ()
setConfig path new = withCfg <&> setPath path new >>= \case
  Left err -> printText err
  Right new' -> do
    saveCfg new'
    printText "Success."

listConfig :: UtActionF ()
listConfig = do
  config <- withCfg

  printText $ T.intercalate "\n" 
    [ formatKV 9 ("platform:", getPath "platform" config)
    , case length config.ecp of
        0 -> formatKV 9 ("ecps:", "None")
        _ -> "ecps:\n" <> formatKVs " - " (M.toAscList $ ecp config)
    ]

getPath :: Text -> UtConfig -> Text
getPath path = go (T.splitOn "." path) where
  go :: [Text] -> UtConfig -> Text
  go ["platform"] c = (show <$> platform c) ?: "Unknown"
  go ["ecp"] c = formatKVs "" (M.toAscList c.ecp)
  go ["ecp", name] c = (show <$> (c.ecp M.!? name )) ?: "Not found"
  go _ _ = "Invalid config (do `ut config list`)"

setPath :: Text -> Text -> UtConfig -> Either Text UtConfig
setPath path new = go (T.splitOn "." path) where
  go :: [Text] -> UtConfig -> Either Text UtConfig
  go ["platform"] c = setPlatform c
  go ["ecp", name] c = setOrDelEcp c name
  go _ _ = fail "Can't set this config path"
  
  setPlatform c = maybeToRight "Expected WSL2 | Mac" $ readMaybe (toString new) <&> \it -> c { platform = it }

  setOrDelEcp c name = Right $ c 
    { ecp = case name of 
        "NULL" -> M.delete name c.ecp
        _      -> M.insert name new c.ecp
    }
