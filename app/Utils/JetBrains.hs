module Utils.JetBrains (jbCmd) where

import Options.Applicative
import Opts
import Utils

jbCmd :: Mod CommandFields Command
jbCmd = mkCommand "jb" "Jetbrains utils" $ JetBrains <$> subparser jbOpts

jbOpts :: Mod CommandFields JetBrainsOpts
jbOpts =
     mkCommand "vim"  "Works with the .ideavim file" vim
  <> mkCommand "open" "Opens an IDE"                open

vim :: Parser JetBrainsOpts
vim = JBVim <$> switch (long "save" <> short 's' <> help "Saves ivim file to template folder, makes/replaces backup of old template")

open :: Parser JetBrainsOpts
open = JBOpen 
  <$> posArg "IDE_NAME" 
  <*> optional (posArg "PATH_TO_OPEN") 
