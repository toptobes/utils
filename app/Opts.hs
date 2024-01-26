{-# LANGUAGE StrictData #-}

module Opts where

newtype Opts = Opts
  { uncommand :: Command
  }

data Command
  = Sync
  | Config    ConfigOpts
  | Haskell   HaskellOpts
  | EchoCP    Text
  | License   LicenseType
  | Obsidian  ObsidianOpts
  | JetBrains JetBrainsOpts

data ConfigOpts
  = GetCfgVal Text
  | SetCfgVal Text Text
  | ListCfg   Bool
  | CfgPath

data HaskellOpts
  = HsHpack HPackOpts
  | HsInit Text HsInitType

data HPackOpts
  = HpackAdd Text
  | HpackDel Text

data HsInitType 
  = Cabal 
  | Hpack

data LicenseType 
  = MIT

data ObsidianOpts
  = ObsidianWithVault Text Text

data JetBrainsOpts
  = JBVim  Bool
  | JBOpen Text (Maybe Text)
