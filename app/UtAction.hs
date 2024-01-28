{-# LANGUAGE TemplateHaskell #-}

module UtAction where

import Control.Monad.Free
import Control.Monad.Free.TH
import System.Process
import Utils
import UtConfig
import Data.Time.Clock
import Data.Time.Calendar

type UtActionF = Free UtAction

type Date = (Integer, Int, Int)

data UtAction next
  = RunSysCmd Text next
  | PrintText Text next
  | WithDate (Date -> next)
  | WithCfgPath  Text (Text -> next)
  | WithCfgFile  Text (Text -> next)
  | WithCfg (UtConfig -> next)
  | SaveCfg  UtConfig next
  deriving (Functor)

$(makeFree ''UtAction)

runActions :: Free UtAction a -> IO a
runActions = iterM go where 
  go (RunSysCmd (toString -> cmd) next) = system cmd >> next
  go (PrintText text next) = putTextLn text >> next
  go (WithDate next) = date >>= next
  go (WithCfgPath path next) = configPath path >>= next
  go (WithCfgFile path next) = configFile path >>= next
  go (WithCfg next) = readConfig >>= next
  go (SaveCfg cfg next) = saveConfig cfg >> next

date :: IO Date
date = getCurrentTime <&> utctDay .- toGregorian
