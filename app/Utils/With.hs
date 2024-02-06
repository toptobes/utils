module Utils.With (withCmd, runWith) where

import Options.Applicative
import UtOpts
import Utils
import UtAction 
import UtConfig
import Data.Map.Strict qualified as M
import Data.Text qualified as T

-- Parsers

withCmd :: Mod CommandFields Command
withCmd = mkCommand "with" "Util to exec commands in a tagged folder" $ With <$> withOpts

withOpts :: Parser WithOpts
withOpts = withFolder <|> listTagged

withFolder :: Parser WithOpts
withFolder = WithFolder 
  <$> posArg "TAG" 
  <*> (T.intercalate " " <$> some (posArg "CMD"))

listTagged :: Parser WithOpts
listTagged = ListTagged <$ switch (long "list" <> short 'l' <> help "Lists tagged folders")

-- Algebras

runWith :: WithOpts -> UtActionF ()
runWith = \case
  (WithFolder name cmd) -> runWithFolder name cmd
  ListTagged -> runListTagged

runWithFolder :: Text -> Text -> UtActionF ()
runWithFolder name cmd =  withCfg <&> with .- M.lookup name >>= \case
  Just path -> runSysCmd $ "cd '" <> path <> "' && " <> cmd
  Nothing -> panik $ "No folder taggeed with name '" <> name <> "' found..."

runListTagged :: UtActionF ()
runListTagged = do
  config <- withCfg
  printText $ formatKVs "" (M.toAscList config.with)
