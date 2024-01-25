{-# LANGUAGE FieldSelectors #-}

module UtConfig where

import Data.Aeson
import Utils

data Platform = WSL2 | Mac
  deriving (Show, Read, Generic, FromJSON, ToJSON)

data UtConfig = UtConfig
  { platform :: Maybe Platform,
    ecp :: Map Text Text
  } deriving (Show, Generic, ToJSON)

instance FromJSON UtConfig where
  parseJSON = withObject "Config" $ \v -> UtConfig
    <$> v .:? "platform"
    <*> v .:? "ecp" .!= fromList []

readConfig :: IO UtConfig
readConfig = do
  path <- configPath
  text <- readFileLBS (toString path)
  pure $! fromMaybe 
    (error "Couldn't decode config—double check it?") 
    (decode text)

saveConfig :: UtConfig -> IO ()
saveConfig cfg = do
  path <- configPath
  writeFileLBS (toString path) $ encode cfg
