module Utils.Config (configCmd, runConfig) where

import Options.Applicative
import UtOpts
import Utils
import UtAction
import UtConfig
import Data.Text qualified as T
import Data.Map.Strict qualified as M
import Data.Aeson

-- Parsers

configCmd :: Mod CommandFields Command
configCmd =  mkCommand "conf" "Works with the config" $ Config <$> subparser configOpts

configOpts :: Mod CommandFields ConfigOpts
configOpts =
     mkCommand "path" "Prints config file path"  ppathConfigOpt
  <> mkCommand "get"  "Lists the current config" listConfigOpt
  <> mkCommand "set"  "Sets the given value"     setConfigOpt

ppathConfigOpt :: Parser ConfigOpts
ppathConfigOpt = pure CfgPath

listConfigOpt :: Parser ConfigOpts
listConfigOpt = fmap ListGetCfg $
      flag' CfgListJSON   (long "json"   <> short 'j' <> help "Shows config as JSON")
  <|> flag' CfgListPretty (long "pretty" <> short 'p' <> help "Shows config prettily")

setConfigOpt :: Parser ConfigOpts
setConfigOpt = 
      ListSetCfg <$ switch (long "list" <> short 'l' <> help "Lists settable options")
  <|> SetCfgVal <$> posArg "CONFIG" <*> posArg "NEW_VAL"

-- Algebras

runConfig :: ConfigOpts -> UtActionF ()
runConfig = \case
  CfgPath -> printConfigPath
  ListGetCfg CfgListJSON -> listConfigJson
  ListGetCfg CfgListPretty -> listConfigPretty
  ListSetCfg -> listConfigSettable
  SetCfgVal path new -> setConfig path new

printConfigPath :: UtActionF ()
printConfigPath = withCfgPath "config.json" >>= printText

setConfig :: Text -> Text -> UtActionF ()
setConfig path new = withCfg <&> setPath path new >>= \case
  Left err -> printText err
  Right new' -> do
    saveCfg new'
    printText "Success."

listConfigJson :: UtActionF ()
listConfigJson = withCfg <&> encode .- decodeUtf8 >>= printText

listConfigPretty :: UtActionF ()
listConfigPretty = do
  config <- withCfg

  printText $ T.intercalate "\n" 
    [ formatKV 9 ("platform:", (show <$> config.platform) ?: "Unknown")
    , case config.repo of
        Repo Nothing Nothing -> formatKV 9 ("repo:", "None")
        Repo {..} -> "repo:\n" <> formatKVs " - " (catMaybes [("path",) <$> path, ("branch",) <$> branch])
    , case length config.ecp of
        0 -> formatKV 9 ("ecps:", "None")
        _ -> "ecps:\n" <> formatKVs " - " (M.toAscList config.ecp)
    , case length config.ecp of
        0 -> formatKV 9 ("vaults:", "None")
        _ -> "vaults:\n" <> formatKVs " - " (M.toAscList config.vaults)
    ]

listConfigSettable :: UtActionF ()
listConfigSettable = printText $ T.intercalate "\n"
  [ "platform: current platform (WSL2 | Mac)"
  , "repo.path: path of git repo to sync with (i.e. toptobes/utils)"
  , "repo.branch: branch of aforementioned repo"
  , "ecp.<key>: KV pair to access & print"
  , "vault.<name>: KV pair to obsidian vaults & paths"
  ]

setPath :: Text -> Text -> UtConfig -> Either Text UtConfig
setPath path new c = go (T.splitOn "." path) where
  go :: [Text] -> Either Text UtConfig
  go ["platform"] = setPlatform
  go ["repo", "path"] = setRepoPath
  go ["repo", "branch"] = setRepoBranch
  go ["ecp", name] = setOrDelEcp name
  go ["vault", name] = setOrDelVault name
  go _ = fail "Can't set this config path"
  
  setPlatform = maybeToRight "Expected WSL2 | Mac" $ readMaybe @Platform (toString new) <&> \it -> c { platform = pure it }

  setRepoPath = pure $ c { repo = Repo (pure new) c.repo.branch }
  setRepoBranch = pure $ c { repo = Repo c.repo.path (pure new) }

  setOrDelEcp name = Right $ c
    { ecp = case name of 
        "NULL" -> M.delete name c.ecp
        _      -> M.insert name new c.ecp
    }

  setOrDelVault name = Right $ c
    { vaults = case name of 
        "NULL" -> M.delete name c.vaults
        _      -> M.insert name new c.vaults
    }
