module Main where

import qualified Main2

-- data Ut w
--   = Hp     { new  :: Maybe Text, add :: Maybe Text, rm :: w ::: Maybe Text <#> "d" }
--   | MIT    { name :: w ::: Text <!> "toptobes"}
--   | Forall { nocp :: Bool }
--   | Lambda { nocp :: Bool }
--   | IVim   { save :: Bool }
--   | OVim   { save :: Bool }
--   | Obsid  { bkup :: Maybe Text, pull :: Maybe Text, diff :: Maybe Text, bsd :: Maybe Bool }
--   | JB     Text (Maybe FilePath)
--   deriving (Generic)

-- deriving instance Show (Ut Unwrapped)

-- instance ParseRecord (Ut Wrapped) where 
--   parseRecord = parseRecordWithModifiers modifiers

-- modifiers :: Modifiers
-- modifiers = defaultModifiers
--   { shortNameModifier = firstLetter
--   }

main :: IO ()
main = Main2.main
  -- ops <- unwrapRecord "ut" <&> \case
  --   Forall {..} -> getText nocp "∀"
  --   Lambda {..} -> getText nocp "λ"

  --   IVim { save = True } -> saveIdeaVim
  --   IVim {} -> printIdeaVim

  --   OVim { save = True } -> saveObsidianVim
  --   OVim {} -> printObsidianVim

  --   MIT {..} -> printMIT name

  --   Hp { new = Just name } -> hpackNew name
  --   Hp { add = Just name } -> hpackAdd name
  --   Hp { rm  = Just name } -> hpackDel name
  --   Hp {} -> error "hpack needs at least one option."

  --   Obsid { bkup = Just name } -> bkupObsidian name
  --   Obsid { pull = Just name } -> pullObsidian name
  --   Obsid { diff = Just name } -> diffObsidian name
  --   Obsid {} -> error "obsidian needs at least one option."

  --   JB ide fp -> jb ide (fp ?: "")

  -- runActions ops

-- getText :: Bool -> Text -> UtActionF ()
-- getText nocopy text = do
--   unless nocopy $
--     runSysCmd ("powershell.exe Set-Clipboard " <> text)
--   printText text

-- saveIdeaVim :: UtActionF ()
-- saveIdeaVim = do
--   vimDir <- withAbsTPath "dotfiles/"
--   runSysCmd $ [r| cp "$(wslpath "$(wslvar USERPROFILE)")/.ideavimrc" |] <> vimDir

-- printIdeaVim :: UtActionF ()
-- printIdeaVim = withTemplate "dotfiles/.ideavimrc" >>= printText

-- saveObsidianVim :: UtActionF ()
-- saveObsidianVim = do
--   vimDir <- withAbsTPath "dotfiles/.obsidian.vimrc"
--   runSysCmd $ "cp " <> vimDir <> [r| "$(wslpath "$(wslvar USERPROFILE)")/Documents/Obsidian Vault/.obsidian.vimrc" |]

-- printObsidianVim :: UtActionF ()
-- printObsidianVim = withTemplate "dotfiles/.obsidian.vimrc" >>= printText

-- printMIT :: Text -> UtActionF ()
-- printMIT name = do
--   year' <- withCurrYear
--   licence <- withTemplate "licences/MIT"

--   printText 
--     $ var "year" (show year') 
--     $ var "name" name
--     licence

-- hpackNew :: Text -> UtActionF ()
-- hpackNew name = do
--   template <- withTemplate "haskell/package.yaml"
--   printText $ var "name" name template

-- hpackAdd :: Text -> UtActionF ()
-- hpackAdd name = runSysCmd command >> hpackRegen where 
--   command = var "name" name [r|awk -i inplace '
--     /^dependencies:/ {
--       in_list = 1 
--     }
--     in_list && match($0, /^(\s+)\S/, a) { 
--       last_item = $0 
--       indent = a[1]
--     }
--     in_list && !/^\s+\S/ && last_item { 
--       print indent "- ${{name}}"
--       in_list = 0
--     }
--     { print }
--   ' package.yaml|]

-- hpackDel :: Text -> UtActionF ()
-- hpackDel name = runSysCmd command >> hpackRegen where
--   command = var "name" name [r|awk -i inplace '
--     !/^\s+-\s*${{name}}/ { 
--       print 
--     }
--   ' package.yaml|]

-- hpackRegen :: UtActionF ()
-- hpackRegen = runSysCmd "hpack"

-- jb :: Text -> FilePath -> UtActionF ()
-- jb ide fp = do
--   path <- withAbsTPath "scripts/jb"
--   runSysCmd $ unwords [path, ide, toText fp]

-- vaults :: HashMap Text Text
-- vaults = fromList [("main", fromWinHome "Documents/Obsidian Vault")]

-- bkupObsidian :: Text -> UtActionF ()
-- bkupObsidian name = case lookup name vaults of
--   Just path -> runSysCmd $ "cd " <> path <> " && git add . && git commit -am 'update' && git push"
--   Nothing -> printText $ "Cannot find vault '" <> name <> "'"

-- pullObsidian :: Text -> UtActionF ()
-- pullObsidian name = case lookup name vaults of
--   Just path -> runSysCmd $ "cd " <> path <> " && git pull"
--   Nothing -> printText $ "Cannot find vault '" <> name <> "'"

-- diffObsidian :: Text -> UtActionF ()
-- diffObsidian name = case lookup name vaults of
--   Just path -> runSysCmd $ "cd " <> path <> " && git diff"
--   Nothing -> printText $ "Cannot find vault '" <> name <> "'"
