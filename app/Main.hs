{-# LANGUAGE QuasiQuotes #-}

module Main where

import Options.Generic
import UtAction
import Text.RawString.QQ
import Utils

data Ut w
  = Hp     { new  :: Maybe Text, add :: w ::: Maybe Text <#> "a", rm :: w ::: Maybe Text <#> "d" }
  | MIT    { name :: w ::: Text <#> "n" <!> "toptobes"}
  | Forall { nocp :: Bool }
  | Lambda { nocp :: Bool }
  | IVim   { save :: w ::: Bool <#> "s" }
  | JB     Text (Maybe FilePath)
  deriving (Generic)

deriving instance Show (Ut Unwrapped)
deriving instance ParseRecord (Ut Wrapped)

main :: IO ()
main = do
  ops <- unwrapRecord "ut" <&> \case
    Forall {..} -> getText nocp "∀"
    Lambda {..} -> getText nocp "λ"

    IVim { save = True } -> saveIVim
    IVim {} -> printIVim

    MIT {..} -> printMIT name

    Hp { new = Just name } -> hpackNew name
    Hp { add = Just name } -> hpackAdd name
    Hp { rm  = Just name } -> hpackDel name
    Hp {} -> error "hpack needs at least one option."

    JB ide fp -> jb ide (fp ?: "")

  runActions ops

getText :: Bool -> Text -> UtActionF ()
getText nocopy text = do
  unless nocopy $
    runSysCmd ("powershell.exe Set-Clipboard " <> text)
  printText text

saveIVim :: UtActionF ()
saveIVim = do
  vimDir <- wAbsTPath "vim/"
  runSysCmd $ [r| cp "$(wslpath "$(wslvar USERPROFILE)")/.IVimrc" |] <> vimDir

printIVim :: UtActionF ()
printIVim = runSysCmd [r| cat "$(wslpath "$(wslvar USERPROFILE)")/.IVimrc" |]

printMIT :: Text -> UtActionF ()
printMIT name = do
  year' <- wCurrYear
  licence <- wTemplate "licences/MIT"

  printText 
    $ var "year" (show year') 
    $ var "name" name
    licence

hpackNew :: Text -> UtActionF ()
hpackNew name = do
  template <- wTemplate "haskell/package.yaml"
  printText $ var "name" name template

hpackAdd :: Text -> UtActionF ()
hpackAdd name = runSysCmd command >> hpackRegen where 
  command = var "name" name [r|awk -i inplace '
    /^dependencies:/ {
      in_list = 1 
    }
    in_list && match($0, /^(\s+)\S/, a) { 
      last_item = $0 
      indent = a[1]
    }
    in_list && !/^\s+\S/ && last_item { 
      print indent "- ${{name}}"
      in_list = 0
    }
    { print }
  ' package.yaml|]

hpackDel :: Text -> UtActionF ()
hpackDel name = runSysCmd command >> hpackRegen where
  command = var "name" name [r|awk -i inplace '
    !/^\s+-\s*${{name}}/ { 
      print 
    }
  ' package.yaml|]

hpackRegen :: UtActionF ()
hpackRegen = runSysCmd "hpack"

jb :: Text -> FilePath -> UtActionF ()
jb ide fp = do
  path <- wAbsTPath "scripts/jb"
  runSysCmd $ unwords [path, ide, toText fp]
