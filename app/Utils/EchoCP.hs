module Utils.EchoCP (ecpCmd) where

import Options.Applicative
import Opts
import Utils

ecpCmd :: Mod CommandFields Command
ecpCmd = mkCommand "ecp" "common text to echo/copy" toEcho

toEcho :: Parser Command
toEcho = EchoCP <$> posArg "DEP"
