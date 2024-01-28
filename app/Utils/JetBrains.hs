module Utils.JetBrains (jbCmd, runJB) where

import Options.Applicative
import UtOpts
import Utils
import UtAction
import UtConfig

-- Parsers

jbCmd :: Mod CommandFields Command
jbCmd = mkCommand "jb" "Jetbrains utils" $ JetBrains <$> subparser jbOpts

jbOpts :: Mod CommandFields JetBrainsOpts
jbOpts =
     mkCommand "vim"  "Works with the .ideavim file"  vimOpts
  <> mkCommand "open" "Opens an IDE"                  open

vimOpts :: Parser JetBrainsOpts
vimOpts = fmap JBVim $ 
      flag' JBVimPath (long "path"  <> short 'p' <> help "Echoes path to .ideavim")
  <|> flag' JBVimEcho (long "print" <> short 'e' <> help "Echoes the content of .ideavim")
  <|> flag' JBVimSave (long "save"  <> short 's' <> help "Saves the .ideavim to /templates, mks backup")

open :: Parser JetBrainsOpts
open = JBOpen
  <$> posArg "IDE_NAME"
  <*> optional (posArg "PATH_TO_OPEN")

-- Algebras

runJB :: JetBrainsOpts -> UtActionF ()
runJB = \case 
  JBOpen name path -> openIDE name path
  JBVim _ -> panik "jb vim command todo lol"

openIDE :: Text -> Maybe Text -> UtActionF ()
openIDE ide fp = withCfg <&> platform >>= \case
  Just WSL2 -> do
    path <- withCfgPath "scripts/jb--wsl.sh"
    runSysCmd $ unwords $ [path, ide] <> maybeToList (toText <$> fp)
  Just Mac -> panik "jb on mac todo lol"
  Nothing -> panik "'platform' needs to be set!"
