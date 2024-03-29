module Utils.EchoCP (ecpCmd, runEcp) where

import Options.Applicative
import UtOpts
import Utils
import UtAction
import UtConfig
import Data.Map.Strict qualified as M

-- Parsers

ecpCmd :: Mod CommandFields Command
ecpCmd = mkCommand "ecp" "Common text to echo/copy" toEcho

toEcho :: Parser Command
toEcho = EchoCP <$> posArg "DEP"

-- Algebras

runEcp :: Text -> UtActionF ()
runEcp key = do
  config <- withCfg

  case config.ecp M.!? key of
    Just val -> printText val >> tryCP config val
    Nothing -> panik "Not found..."

tryCP :: UtConfig -> Text -> UtActionF ()
tryCP UtConfig { platform } text = case platform of
  Just WSL2 -> runSysCmd ("powershell.exe Set-Clipboard " <> text)
  Just Mac -> runSysCmd ("echo '" <> text <> "' | pbcopy")
  Nothing -> panik "set platform to use clipboard"
