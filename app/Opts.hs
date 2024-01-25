{-# LANGUAGE StrictData #-}

module Opts where

newtype Opts = Opts
  { uncommand :: Command
  }

data Command
  = Sync
  | Config    ConfigOpts
  | Hpack     HPackOpts
  | EchoCP    Text
  | License   LicenseType
  | Obsidian  ObsidianOpts
  | JetBrains JetBrainsOpts

data ConfigOpts
  = GetCfgVal Text
  | SetCfgVal Text Text
  | ListCfg
  | CfgPath

data HPackOpts
  = HpackAdd Text
  | HpackDel Text
  | HpackNew Text

data LicenseType 
  = MIT

data ObsidianOpts
  = ObsidianWithVault Text Text

data JetBrainsOpts
  = JBVim  Bool
  | JBOpen Text (Maybe Text)
