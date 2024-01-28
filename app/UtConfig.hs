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
  , vaults   :: Map Text Text
  } deriving (Show, Generic, ToJSON)

data Repo = Repo 
  { path   :: Maybe Text
  , branch :: Maybe Text
  } deriving (Show, Read, Generic, ToJSON)

instance FromJSON UtConfig where
  parseJSON = withObject "Config" $ \v -> UtConfig
    <$> v .:? "platform"
    <*> v .:? "repo"   .!= Repo Nothing Nothing
    <*> v .:? "ecp"    .!= fromList []
    <*> v .:? "vaults" .!= fromList []

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \v -> Repo
    <$> v .:? "path"
    <*> v .:? "branch"

readConfig :: IO UtConfig
readConfig = do
  path <- configPath "config.json"
  text <- readFileLBS (toString path)
  pure $! fromMaybe
    (panik "Couldn't decode config—double check it?")
    (decode text)

saveConfig :: UtConfig -> IO ()
saveConfig cfg = do
  path <- configPath "config.json"
  writeFileLBS (toString path) $ encode cfg

configFile :: Text -> IO Text
configFile = configPath >=> (toString .- readFileBS <&> fmap decodeUtf8)

configPath :: Text -> IO Text
configPath rel = do
  dir <- getXdgDirectory XdgConfig "./toptobes-utils/"
  
  unlessM (doesDirectoryExist dir) $
    panik "run 'ut sync --init' first..."

  pure $ toText dir <> rel
