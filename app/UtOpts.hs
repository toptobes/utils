{-# LANGUAGE StrictData #-}

module UtOpts where

newtype UtOpts = UtOpts
  { uncommand :: Command
  }

data Command
  = Sync      SyncOpts
  | Config    ConfigOpts
  | Haskell   HaskellOpts
  | EchoCP    Text
  | License   LicenseOpts
  | With      WithOpts
  | JetBrains JetBrainsOpts

data SyncOpts
  = SyncPush
  | SyncPull
  | SyncInit

data ConfigOpts
  = SetCfgVal Text Text
  | ListGetCfg CfgListType
  | ListSetCfg
  | CfgPath

data CfgListType 
  = CfgListPretty
  | CfgListJSON

data HaskellOpts
  = HsHpack HPackOpts
  | HsInit Text HsInitType

data HPackOpts
  = HpackAdd Text
  | HpackDel Text

data HsInitType 
  = Cabal 
  | Hpack

data LicenseOpts
  = PrintLicense LicenseType Text

data LicenseType 
  = MIT

data WithOpts
  = WithFolder Text Text
  | ListTagged

data JetBrainsOpts
  = JBVim  JBVimOpts
  | JBOpen Text (Maybe Text)

data JBVimOpts
  = JBVimPath
  | JBVimSave
  | JBVimEcho
