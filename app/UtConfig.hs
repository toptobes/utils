{-# LANGUAGE FieldSelectors #-}

module UtConfig where

import Data.Aeson
import Utils
import System.Directory

data Platform = WSL2 | Mac
  deriving (Show, Read, Generic, FromJSON, ToJSON)

data UtConfig = UtConfig
  { platform :: Maybe Platform
  , repo     :: Repo
  , ecp      :: Map Text Text
  , with   :: Map Text Text
  } deriving (Show, Generic)

data Repo = Repo
  { path   :: Maybe Text
  , branch :: Maybe Text
  } deriving (Show, Read, Generic, ToJSON)

newtype Local = Local  UtConfig
newtype Share = Share UtConfig

instance FromJSON UtConfig where
  parseJSON = withObject "Config" $ \v -> UtConfig
    <$> v .:? "platform"
    <*> v .:? "repo"   .!= Repo Nothing Nothing
    <*> v .:? "ecp"    .!= fromList []
    <*> v .:? "with" .!= fromList []

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo
    <$> v .:? "path"
    <*> v .:? "branch"

instance ToJSON Local where
  toJSON (Local UtConfig {..}) = object
    [ "platform" .= platform
    , "with"   .= with
    ]

instance ToJSON Share where
  toJSON (Share UtConfig {..}) = object
    [ "ecp"  .= ecp
    , "repo" .= repo
    ]

readConfig :: IO UtConfig
readConfig = do
  localCfg <- mkConfig "config.local.json"
  shareCfg <- mkConfig "config.share.json"

  pure $ UtConfig
    { platform = localCfg.platform
    , with   = localCfg.with
    , ecp  = shareCfg.ecp
    , repo = shareCfg.repo
    }
  where
    mkConfig :: Text -> IO UtConfig
    mkConfig file = do
      path <- configPath file
      text <- readFileLBS (toString path)
      pure $! fromMaybe
        (panik $ "Couldn't decode '" <> file <> "'—double check it?")
        (decode text)

saveConfig :: UtConfig -> IO ()
saveConfig cfg = do
  go (Local cfg) "config.local.json"
  go (Share cfg) "config.share.json"
  where
    go :: (ToJSON w) => w -> Text -> IO ()
    go cfg' file = do
      path <- configPath file
      writeFileLBS (toString path) $ encode cfg'

configFile :: Text -> IO Text
configFile = configPath >=> (toString .- readFileBS <&> fmap decodeUtf8)

configPath :: Text -> IO Text
configPath rel = do
  dir <- getXdgDirectory XdgConfig "./toptobes-utils/"

  unlessM (doesDirectoryExist dir) $
    panik "run 'ut sync --init' first..."

  pure $ toText dir <> rel
