{-# LANGUAGE FieldSelectors #-}

module UtConfig where

import Data.Aeson
import Utils
import Data.Text qualified as T
import Data.Map.Strict qualified as M

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
    (panik "Couldn't decode config—double check it?")
    (decode text)

saveConfig :: UtConfig -> IO ()
saveConfig cfg = do
  path <- configPath
  writeFileLBS (toString path) $ encode cfg

require :: (Monad m) => [Text] -> UtConfig -> m UtConfig
require paths cfg = cfg <$ pure (go <$> paths) where
  go path = if not $ cfg `has` path
    then panik $ "'" <> path <> "' needs to be set to use this command"
    else ()

has :: UtConfig -> Text -> Bool
has cfg path = go (T.splitOn "." path) cfg  where
  go :: [Text] -> UtConfig -> Bool
  go ["platform"] c = isJust $ platform c
  go ["ecp", name] c = isJust $ c.ecp M.!? name
  go _ _ = False
