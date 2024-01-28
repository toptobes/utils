module Utils.License (licenseCmd, runLicense) where

import Options.Applicative
import UtOpts
import Utils
import UtAction 

-- Parsers

licenseCmd :: Mod CommandFields Command
licenseCmd = mkCommand "li" "License utils" $ License <$> licenseOpts

licenseOpts :: Parser LicenseOpts
licenseOpts = PrintLicense
  <$> flag' MIT (long "mit")
  <*> strOption (long "name" <> short 'n' <> metavar "NAME" <> help "Name of license holder")

-- Algebras

runLicense :: LicenseOpts -> UtActionF ()
runLicense (PrintLicense MIT name) = do
  year' <- withDate <&> \(y, _, _) -> y
  licence <- withCfgFile "templates/licenses/MIT"

  printText 
    $ var "year" (show year') 
    $ var "name" name
    licence
