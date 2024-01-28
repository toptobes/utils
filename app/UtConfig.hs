{-# LANGUAGE FieldSelectors #-}

module UtConfig where

import Data.Aeson
import Utils

data Platform = WSL2 | Mac
  deriving (Show, Read, Generic, FromJSON, ToJSON)

data UtConfig = UtConfig
  { platform :: Maybe Platform
  , repo :: Repo
  , ecp :: Map Text Text
  } deriving (Show, Generic, ToJSON)

data Repo = Repo 
  { path :: Maybe Text
  , branch :: Maybe Text
  } deriving (Show, Read, Generic, ToJSON)

instance FromJSON UtConfig where
  parseJSON = withObject "Config" $ \v -> UtConfig
    <$> v .:? "platform"
    <*> v .:? "repo" .!= Repo Nothing Nothing
    <*> v .:? "ecp"  .!= fromList []

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
