{-# LANGUAGE FieldSelectors #-}

module UtConfig where

import Data.Aeson
import Utils
import Data.Text qualified as T
import Data.List

data Platform = WSL2 | Mac
  deriving (Show, Generic, FromJSON, ToJSON)

data UTConfig = UTConfig
  { platform :: Maybe Platform,
    ecp :: [(Text, Text)]
  } deriving (Show, Generic, ToJSON)

instance FromJSON UTConfig where
  parseJSON = withObject "Config" $ \v -> UTConfig
    <$> v .:? "platform"
    <*> v .:? "ecp" .!= []

readConfig :: IO UTConfig
readConfig = do
  path <- configPath
  text <- readFileLBS (toString path)
  pure $ fromMaybe 
    (error "Couldn't decode config—double check it?") 
    (decode text)

saveConfig :: UTConfig -> IO ()
saveConfig cfg = do
  path <- configPath
  writeFileLBS (toString path) $ encode cfg

lookupCfg :: Text -> UTConfig -> Text
lookupCfg path = go (T.splitOn "." path) where
  go :: [Text] -> UTConfig -> Text
  go ["platform"] c = (show <$> platform c) ?: "Unknown"
  go ["ecp"] c = show $ ecp c
  go ["ecp", name] c = (show <$> lookup name (ecp c)) ?: "Not found"
  go _ _ = "Invalid config (do `ut config list`)"
